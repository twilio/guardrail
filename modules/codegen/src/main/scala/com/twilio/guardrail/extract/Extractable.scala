package com.twilio.guardrail.extract

import com.twilio.guardrail.{ DataRedacted, DataVisible, EmptyIsEmpty, EmptyIsNull, EmptyToNullBehaviour, RedactionBehaviour }
import scala.util.{ Success, Try }
import scala.reflect.ClassTag
import scala.collection.JavaConverters._
import java.util

trait Extractable[T] {
  def extract(v: Any): Try[T]
}

object Extractable {
  private[this] def buildExceptionally[T](f: PartialFunction[Any, Try[T]]): Extractable[T] =
    (v: Any) => Try(f(v)).flatten

  def build[T](f: PartialFunction[Any, T]): Extractable[T] =
    buildExceptionally(f.andThen(Success(_)))

  implicit val defaultExtractableBoolean: Extractable[Boolean] =
    build[Boolean]({ case x: Boolean => x })
  implicit val defaultExtractableDouble: Extractable[Double] = build[Double]({
    case x: Float                => x.toDouble
    case x: Double               => x
    case x: java.math.BigDecimal => x.doubleValue
    case x: BigDecimal           => x.doubleValue
  })
  implicit val defaultExtractableFloat: Extractable[Float] = build[Float]({
    case x: Float                => x
    case x: Double               => x.toFloat // FIXME: Will likely trim
    case x: java.math.BigDecimal => x.floatValue
    case x: BigDecimal           => x.floatValue
  })
  implicit val defaultExtractableInt: Extractable[Int] = build[Int]({
    case x: Int                                                    => x
    case x: Long if x <= Int.MaxValue && x >= Int.MinValue         => x.toInt // FIXME: Warn that default value is being dropped
    case x: java.math.BigInteger if Try(x.intValueExact).isSuccess => x.intValueExact
    case x: BigInt if x.isValidInt                                 => x.intValue
  })
  implicit val defaultExtractableLong: Extractable[Long] = build[Long]({
    case x: Int                                                     => x
    case x: Long                                                    => x
    case x: java.math.BigInteger if Try(x.longValueExact).isSuccess => x.longValueExact
    case x: BigInt if x.isValidLong                                 => x.intValue
  })
  implicit val defaultExtractableString: Extractable[String] = build[String]({
    case x: String => x
  })
  implicit val defaultExtractableEmptyToNullBehaviour: Extractable[EmptyToNullBehaviour] =
    build[EmptyToNullBehaviour]({
      case x: Boolean if x  => EmptyIsNull
      case x: Boolean if !x => EmptyIsEmpty
    })
  implicit val defaultExtractableRedactionBehaviour: Extractable[RedactionBehaviour] =
    build[RedactionBehaviour]({
      case x: Boolean if x  => DataRedacted
      case x: Boolean if !x => DataVisible
    })

  implicit def defaultExtractableList[T: Extractable: ClassTag]: Extractable[List[T]] =
    buildExceptionally[List[T]](validateSeq.andThen(xs => Try(validateListItems[T].apply(xs))))

  private[this] def validateSeq: PartialFunction[Any, List[_]] = {
    case x: Seq[_]       => x.toList
    case x: util.List[_] => x.asScala.toList
  }

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  private[this] def validateListItems[A](implicit cls: ClassTag[A]): PartialFunction[List[_], List[A]] = {
    case xs: List[_] if xs.forall(x => cls.runtimeClass.isAssignableFrom(x.getClass)) => xs.asInstanceOf[List[A]]
  }
}
