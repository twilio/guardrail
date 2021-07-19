package dev.guardrail.generators.syntax

import cats.syntax.all._
import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.comments.{ BlockComment, Comment }
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.nodeTypes.{ NodeWithName, NodeWithSimpleName }
import com.github.javaparser.ast.{ CompilationUnit, ImportDeclaration, Node, NodeList }
import dev.guardrail.languages.JavaLanguage
import dev.guardrail.languages.JavaLanguage.JavaTypeName
import dev.guardrail.{ RuntimeFailure, SupportDefinition, Target }

import scala.collection.JavaConverters._
import scala.compat.java8.OptionConverters._
import scala.reflect.ClassTag
import scala.util.{ Failure, Success, Try }

object Java {
  implicit class RichType(private val tpe: Type) extends AnyVal {
    def containedType: Type =
      tpe match {
        case cls: ClassOrInterfaceType => cls.getTypeArguments.asScala.filter(_.size == 1).fold(tpe)(_.get(0))
        case _                         => tpe
      }

    def box: Type =
      tpe match {
        case pt: PrimitiveType => pt.toBoxedType
        case _                 => tpe
      }

    def unbox: Type =
      tpe match {
        case cls: ClassOrInterfaceType if cls.isBoxedType => cls.toUnboxedType
        case _                                            => tpe
      }

    def isNamed(name: String): Boolean =
      tpe match {
        case cls: ClassOrInterfaceType if name.contains(".") =>
          (cls.getScope.asScala.fold("")(_.getName.asString + ".") + cls.getNameAsString) == name
        case cls: ClassOrInterfaceType => cls.getNameAsString == name
        case other                     => other.asString == name
      }

    // Heuristicly guess if the type could be reasonably used as a text/plain body
    def isPlain: Boolean =
      tpe.isPrimitiveType || tpe.isNamed("BigInteger") || tpe.isNamed("BigDecimal") || tpe.isNamed("String") || tpe.isNamed("OffsetDateTime") || tpe.isNamed(
          "LocalDate"
        )

    @deprecated("Just use Type#asString", "0.0.0")
    def name: Option[String] = Option(tpe.asString)
  }

  implicit class RichNode(private val n: Node) extends AnyVal {
    def toExpression: Target[Expression] = n match {
      case expr: Expression => Target.pure(expr)
      case _                => Target.raiseError(RuntimeFailure(s"Node $n cannot be converted to an Expression"))
    }
  }

  implicit class RichListOfNode[T <: Node](private val l: List[T]) extends AnyVal {
    def toNodeList: NodeList[T] = new NodeList[T](l: _*)
  }

  implicit class RichNodeList[T <: Node](private val nl: NodeList[T]) extends AnyVal {
    def toList(implicit cls: ClassTag[T]): List[T] = nl.iterator.asScala.toList
  }

  def formatException[T <: Throwable](prefix: String): T => String = {
    case t: com.github.javaparser.ParseProblemException =>
      val problems     = t.getProblems().asScala.toVector
      val msgSeparator = if (problems.length > 1) "\n" else " "
      val msgs = problems
        .map(
          problem =>
            problem.getCause.asScala
              .flatMap({
                case cause: com.github.javaparser.ParseException =>
                  val tokenImage = cause.tokenImage.toVector
                  val expected   = Option(cause.expectedTokenSequences).map(_.toVector.flatMap(_.toVector)).orEmpty.flatMap(idx => tokenImage.get(idx))
                  for {
                    token       <- Option(cause.currentToken)
                    nextToken   <- Option(token.next)
                    image       <- Option(nextToken.image)
                    beginColumn <- Option(nextToken.beginColumn)
                  } yield s"""Unexpected "${image}" at character ${beginColumn} (valid: ${expected.mkString(", ")})"""
                case _ => Option.empty
              })
              .getOrElse(problem.getMessage())
        )
      val msg = msgs match {
        case Vector()    => "\n" + t.getMessage()
        case Vector(msg) => msg
        case rest =>
          rest.zipWithIndex
            .map({
              case (msg, idx) =>
                s"""Problem ${idx + 1}:
                  |  ${msg.trim.split("\n").mkString("\n  ")}
                  |""".stripMargin
            })
            .mkString("\n")
      }
      s"${prefix}:${msgSeparator}${msg}"
    case t =>
      s"${prefix}: ${t.getMessage}"
  }

  private[this] def safeParse[T](log: String)(parser: String => T, s: String)(implicit cls: ClassTag[T]): Target[T] =
    Target.log.function(s"${log}: ${s}") {
      Try(parser(s)) match {
        case Success(value) => Target.pure(value)
        case Failure(t)     => Target.raiseUserError(formatException(s"Unable to parse '${s}' to a ${cls.runtimeClass.getName}")(t))
      }
    }

  def safeParseCode(s: String): Target[CompilationUnit]  = safeParse("safeParseCode")(StaticJavaParser.parse, s)
  def safeParseSimpleName(s: String): Target[SimpleName] = safeParse("safeParseSimpleName")(StaticJavaParser.parseSimpleName, s)
  def safeParseName(s: String): Target[Name]             = safeParse("safeParseName")(StaticJavaParser.parseName, s)
  def safeParseType(s: String): Target[Type]             = safeParse("safeParseType")(StaticJavaParser.parseType, s)
  def safeParseTypeName(s: String): Target[JavaTypeName] = safeParse("safeParseTypeName")(s => JavaTypeName(StaticJavaParser.parseType(s)), s)
  def safeParseClassOrInterfaceType(s: String): Target[ClassOrInterfaceType] =
    safeParse("safeParseClassOrInterfaceType")(StaticJavaParser.parseClassOrInterfaceType, s)
  def safeParseExpression[T <: Expression](s: String)(implicit cls: ClassTag[T]): Target[T] =
    safeParse[T]("safeParseExpression")(StaticJavaParser.parseExpression[T], s)
  def safeParseParameter(s: String): Target[Parameter]               = safeParse("safeParseParameter")(StaticJavaParser.parseParameter, s)
  def safeParseImport(s: String): Target[ImportDeclaration]          = safeParse("safeParseImport")(StaticJavaParser.parseImport, s)
  def safeParseRawImport(s: String): Target[ImportDeclaration]       = safeParse("safeParseRawImport")(StaticJavaParser.parseImport, s"import ${s};")
  def safeParseRawStaticImport(s: String): Target[ImportDeclaration] = safeParse("safeParseStaticImport")(StaticJavaParser.parseImport, s"import static ${s};")

  def functionType(in: Type, out: Type): ClassOrInterfaceType = StaticJavaParser.parseClassOrInterfaceType("Function").setTypeArguments(in, out)
  def supplierType(of: Type): ClassOrInterfaceType            = StaticJavaParser.parseClassOrInterfaceType("Supplier").setTypeArguments(of)

  val VOID_TYPE: ClassOrInterfaceType            = StaticJavaParser.parseClassOrInterfaceType("Void")
  val OBJECT_TYPE: ClassOrInterfaceType          = StaticJavaParser.parseClassOrInterfaceType("Object")
  val STRING_TYPE: ClassOrInterfaceType          = StaticJavaParser.parseClassOrInterfaceType("String")
  val THROWABLE_TYPE: ClassOrInterfaceType       = StaticJavaParser.parseClassOrInterfaceType("Throwable")
  val ASSERTION_ERROR_TYPE: ClassOrInterfaceType = StaticJavaParser.parseClassOrInterfaceType("AssertionError")

  private def nameFromExpr(expr: Expression): String = expr match {
    case _: ThisExpr                  => "this"
    case ce: ClassExpr                => s"${ce.getType.toString}.class"
    case fae: FieldAccessExpr         => s"${nameFromExpr(fae.getScope)}.${fae.getNameAsString}"
    case nwsn: NodeWithSimpleName[_]  => nwsn.getNameAsString
    case nwn: NodeWithName[_]         => nwn.getNameAsString
    case lsve: LiteralStringValueExpr => lsve.getValue
    case other                        => other.toString
  }

  def requireNonNullExpr(param: Expression): Expression = new MethodCallExpr(
    "requireNonNull",
    param,
    new StringLiteralExpr(s"${nameFromExpr(param)} is required")
  )

  def requireNonNullExpr(paramName: String): Expression = requireNonNullExpr(new NameExpr(paramName))

  def optionalOfExpr(param: Expression): Expression = new MethodCallExpr(
    new NameExpr("java.util.Optional"),
    "of",
    new NodeList[Expression](
      requireNonNullExpr(param)
    )
  )

  def buildMethodCall(name: String, arg: Option[Node] = None): Target[Node] = arg match {
    case Some(expr: Expression) => Target.pure(new MethodCallExpr(name, expr))
    case None                   => Target.pure(new MethodCallExpr(name))
    case other                  => Target.raiseUserError(s"Need expression to call '${name}' but got a ${other.getClass.getName} instead")
  }

  val GENERATED_CODE_COMMENT: Comment = new BlockComment(GENERATED_CODE_COMMENT_LINES.mkString("\n * ", "\n * ", "\n"))

  private val reservedMethodNames = Set(
    "getClass"
  )

  def getterMethodNameForParameter(parameterName: String): String = {
    val name = s"get${parameterName.unescapeIdentifier.capitalize}"
    if (reservedMethodNames.contains(name)) {
      name + "_"
    } else {
      name
    }
  }

  // from https://en.wikipedia.org/wiki/List_of_Java_keywords
  private val reservedWords = Set(
    "abstract",
    "assert",
    "boolean",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "exports",
    "extends",
    "false",
    "final",
    "finally",
    "float",
    "for",
    "goto",
    "if",
    "implements",
    "import",
    "instanceof",
    "int",
    "interface",
    "long",
    "module",
    "native",
    "new",
    "null",
    "package",
    "private",
    "protected",
    "public",
    "requires",
    "return",
    "short",
    "static",
    "strictfp",
    "super",
    "switch",
    "synchronized",
    "this",
    "throw",
    "throws",
    "transient",
    "true",
    "try",
    "var",
    "void",
    "volatile",
    "while"
  )

  implicit class RichJavaString(private val s: String) extends AnyVal {
    def escapeInvalidCharacters: String =
      s.map({
          case '`'  => "_backtick_"
          case '~'  => "_tilde_"
          case '!'  => "_bang_"
          case '@'  => "_at_"
          case '#'  => "_pound_"
          case '%'  => "_percent_"
          case '^'  => "_caret_"
          case '&'  => "_ampersand_"
          case '*'  => "_asterisk_"
          case '('  => "_open_paren_"
          case ')'  => "_close_paren_"
          case '-'  => "_"
          case '+'  => "_plus_"
          case '='  => "_equals_"
          case '|'  => "_pipe_"
          case ':'  => "_colon_"
          case ';'  => "_semicolon_"
          case '"'  => "_double_quote_"
          case '\'' => "_single_quote_"
          case ','  => "_comma_"
          case '.'  => "_dot_"
          case '?'  => "_question_"
          case '\\' => "_backslash_"
          case '/'  => "_slash_"
          case '['  => "_open_square_brace_"
          case ']'  => "_close_square_brace_"
          case '{'  => "_open_curly_brace_"
          case '}'  => "_close_curly_brace_"
          case '<'  => "_less_than_"
          case '>'  => "_greater_than_"
          case c    => c.toString
        })
        .mkString
        .replaceAll("^_+", "")
        .replaceAll("_+$", "")

    def escapeReservedWord: String = if (reservedWords.contains(s)) s + "_" else s
    def unescapeReservedWord: String =
      if (s.endsWith("_")) {
        val prefix = s.substring(0, s.length - 1)
        if (reservedWords.contains(prefix)) {
          prefix
        } else {
          s
        }
      } else {
        s
      }

    def escapeIdentifier: String = {
      val reservedEscaped = s.escapeReservedWord
      if (reservedEscaped.nonEmpty && reservedEscaped.charAt(0) >= '0' && reservedEscaped.charAt(0) <= '9') "_" + reservedEscaped
      else reservedEscaped
    }

    def unescapeIdentifier: String = {
      val removedLeadingUnderscore =
        if (s.startsWith("_") && s.length >= 2 && s.charAt(1) >= '0' && s.charAt(1) <= '9') s.substring(1)
        else s
      removedLeadingUnderscore.unescapeReservedWord
    }
  }

  def sortDefinitions(defns: List[BodyDeclaration[_ <: BodyDeclaration[_]]]): List[BodyDeclaration[_ <: BodyDeclaration[_]]] = {
    import com.github.javaparser.ast.Modifier.Keyword._
    def sortKeyFor(x: BodyDeclaration[_ <: BodyDeclaration[_]]): Int = x match {
      case cd: ClassOrInterfaceDeclaration if cd.getModifiers.contains(PUBLIC)                              => 0
      case cd: ClassOrInterfaceDeclaration if cd.getModifiers.contains(PROTECTED)                           => 10
      case cd: ClassOrInterfaceDeclaration if !cd.getModifiers.contains(PRIVATE)                            => 20
      case _: ClassOrInterfaceDeclaration                                                                   => 30
      case fd: FieldDeclaration if fd.getModifiers.contains(PUBLIC) && fd.getModifiers.contains(PUBLIC)     => 40
      case fd: FieldDeclaration if fd.getModifiers.contains(STATIC) && fd.getModifiers.contains(PROTECTED)  => 50
      case fd: FieldDeclaration if fd.getModifiers.contains(STATIC) && !fd.getModifiers.contains(PRIVATE)   => 60
      case fd: FieldDeclaration if fd.getModifiers.contains(STATIC)                                         => 70
      case _: InitializerDeclaration                                                                        => 80
      case fd: FieldDeclaration if fd.getModifiers.contains(PUBLIC)                                         => 90
      case fd: FieldDeclaration if fd.getModifiers.contains(PROTECTED)                                      => 100
      case fd: FieldDeclaration if !fd.getModifiers.contains(PRIVATE)                                       => 110
      case _: FieldDeclaration                                                                              => 120
      case md: MethodDeclaration if md.getModifiers.contains(STATIC) && md.getModifiers.contains(PUBLIC)    => 130
      case md: MethodDeclaration if md.getModifiers.contains(STATIC) && md.getModifiers.contains(PROTECTED) => 140
      case md: MethodDeclaration if md.getModifiers.contains(STATIC) && !md.getModifiers.contains(PRIVATE)  => 150
      case md: MethodDeclaration if md.getModifiers.contains(STATIC)                                        => 160
      case cd: ConstructorDeclaration if cd.getModifiers.contains(PUBLIC)                                   => 170
      case cd: ConstructorDeclaration if cd.getModifiers.contains(PROTECTED)                                => 180
      case cd: ConstructorDeclaration if !cd.getModifiers.contains(PRIVATE)                                 => 190
      case _: ConstructorDeclaration                                                                        => 200
      case md: MethodDeclaration if md.getModifiers.contains(PUBLIC)                                        => 210
      case md: MethodDeclaration if md.getModifiers.contains(PROTECTED)                                     => 220
      case md: MethodDeclaration if !md.getModifiers.contains(PRIVATE)                                      => 230
      case _: MethodDeclaration                                                                             => 240
      case _                                                                                                => 250
    }
    defns.sortWith((a, b) => sortKeyFor(a) - sortKeyFor(b) < 0)
  }

  def loadSupportDefinitionFromString(className: String, source: String): Target[SupportDefinition[JavaLanguage]] =
    Try(StaticJavaParser.parse(source)) match {
      case Failure(t) =>
        Target.raiseUserError[SupportDefinition[JavaLanguage]](s"Failed to parse class ${className} from string: $t")
      case Success(cu) =>
        cu.getClassByName(className)
          .asScala
          .orElse(cu.getInterfaceByName(className).asScala)
          .fold(
            Target.raiseUserError[SupportDefinition[JavaLanguage]](s"Unable to find class ${className} in parsed string")
          )(
            clsDef =>
              Target.pure(
                SupportDefinition[JavaLanguage](
                  new Name(className),
                  cu.getImports.toList,
                  List(clsDef)
                )
              )
          )
    }

  def generatedAnnotation(clazz: Class[_]) = new NormalAnnotationExpr(
    new Name("javax.annotation.Generated"),
    new NodeList(
      new MemberValuePair(
        "value",
        new StringLiteralExpr(clazz.getName)
      )
    )
  )

  def generatedAnnotationString(clazz: Class[_]) = s"""@javax.annotation.Generated("${clazz.getName}")"""
}
