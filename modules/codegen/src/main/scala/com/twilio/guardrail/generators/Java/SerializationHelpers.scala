package com.twilio.guardrail.generators.Java

import com.twilio.guardrail.{ SupportDefinition, Target }
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage

object SerializationHelpers {
  def showerSupportDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "Shower",
    """
      public class Shower {
          @SuppressWarnings("serial")
          public static class UnshowableInstanceException extends RuntimeException {
              public UnshowableInstanceException(final Object instance) {
                  super("Instance of type " + instance.getClass().getName() + " is not showable");
              }
          }

          public interface Showable<T> {
              String show(T value);
          }

          private static final Shower instance = new Shower();

          public static Shower getInstance() {
              return instance;
          }

          private final java.util.Map<Class<?>, Showable<?>> showables = new java.util.concurrent.ConcurrentHashMap<>();

          private Shower() {
              registerDefaultInstances();
          }

          public <T> void register(final Class<T> cls, final Showable<T> showable) {
              this.showables.put(cls, showable);
          }

          @SuppressWarnings("unchecked")
          public String show(final Object value) {
              return show(value, (Class<Object>)value.getClass());
          }

          public boolean canShow(final Class<?> cls) {
              if (this.showables.containsKey(cls)) {
                  return true;
              } else {
                  final Class<?> superclass = cls.getSuperclass();
                  if (superclass != null) {
                      return canShow(superclass);
                  } else {
                      return false;
                  }
              }
          }

          @SuppressWarnings("unchecked")
          private String show(final Object value, final Class<Object> cls) {
              if (this.showables.containsKey(cls)) {
                  final Showable<Object> showable = (Showable<Object>)this.showables.get(cls);
                  return showable.show(value);
              } else {
                  final Class<Object> superclass = cls.getSuperclass();
                  if (superclass != null) {
                      return show(value, superclass);
                  } else {
                      throw new UnshowableInstanceException(value);
                  }
              }
          }

          private void registerDefaultInstances() {
              register(Boolean.class, String::valueOf);
              register(Byte.class, String::valueOf);
              register(Character.class, String::valueOf);
              register(Short.class, String::valueOf);
              register(Integer.class, String::valueOf);
              register(Long.class, String::valueOf);
              register(java.math.BigInteger.class, java.math.BigInteger::toString);
              register(Float.class, String::valueOf);
              register(Double.class, String::valueOf);
              register(java.math.BigDecimal.class, java.math.BigDecimal::toString);
              register(String.class, value -> value);
              register(java.time.LocalDate.class, value -> value.format(java.time.format.DateTimeFormatter.ISO_DATE));
              register(java.time.OffsetDateTime.class, value -> value.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME));
              register(java.net.URL.class, java.net.URL::toString);
              register(java.net.URI.class, java.net.URI::toString);
              register(java.util.UUID.class, java.util.UUID::toString);
          }
      }
    """
  )

  def guardrailJerseySupportDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "GuardrailJerseySupport",
    """
      import io.dropwizard.jersey.params.AbstractParam;
      import io.dropwizard.setup.Bootstrap;
      import io.dropwizard.setup.Environment;
      import org.glassfish.hk2.utilities.binding.AbstractBinder;

      import javax.annotation.Nullable;
      import javax.ws.rs.BadRequestException;
      import java.time.Duration;
      import java.time.Instant;
      import java.time.LocalDate;
      import java.time.LocalDateTime;
      import java.time.LocalTime;
      import java.time.OffsetDateTime;
      import java.time.OffsetTime;
      import java.time.ZonedDateTime;
      import java.util.Objects;

      public class GuardrailJerseySupport {
          public static class Jsr310 {
              public abstract static class GuardrailAbstractParam<T> extends AbstractParam<T> {
                  private final T value;

                  @SuppressWarnings("unused")
                  protected GuardrailAbstractParam(@Nullable final String input) {
                      this(input, "Parameter");
                  }

                  protected GuardrailAbstractParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                      try {
                          this.value = realParse(input);
                      } catch (final Exception e) {
                          throw new BadRequestException(String.format("%s is invalid: %s", parameterName, input));
                      }
                  }

                  @Override
                  protected T parse(@Nullable final String input) {
                      return null;
                  }

                  protected abstract T realParse(@Nullable final String input) throws Exception;

                  @Override
                  public T get() {
                      return this.value;
                  }

                  @Override
                  public boolean equals(final Object obj) {
                      if (this == obj) {
                          return true;
                      } else if (getClass() != obj.getClass()) {
                          return false;
                      } else {
                          return this.value.equals(((GuardrailAbstractParam<?>) obj).value);
                      }
                  }

                  @Override
                  public int hashCode() {
                      return Objects.hashCode(this.value);
                  }

                  @Override
                  public String toString() {
                      return this.value != null ? this.value.toString() : "(null)";
                  }
              }

              @SuppressWarnings("unused")
              public static class InstantParam extends GuardrailAbstractParam<Instant> {
                  public InstantParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected Instant realParse(final String input) {
                      return Instant.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class OffsetDateTimeParam extends GuardrailAbstractParam<OffsetDateTime> {
                  public OffsetDateTimeParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected OffsetDateTime realParse(final String input) {
                      return OffsetDateTime.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class ZonedDateTimeParam extends GuardrailAbstractParam<ZonedDateTime> {
                  public ZonedDateTimeParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected ZonedDateTime realParse(final String input) {
                      return ZonedDateTime.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class LocalDateTimeParam extends GuardrailAbstractParam<LocalDateTime> {
                  public LocalDateTimeParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected LocalDateTime realParse(final String input) {
                      return LocalDateTime.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class LocalDateParam extends GuardrailAbstractParam<LocalDate> {
                  public LocalDateParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected LocalDate realParse(final String input) {
                      return LocalDate.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class LocalTimeParam extends GuardrailAbstractParam<LocalTime> {
                  public LocalTimeParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected LocalTime realParse(final String input) {
                      return LocalTime.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class OffsetTimeParam extends GuardrailAbstractParam<OffsetTime> {
                  public OffsetTimeParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected OffsetTime realParse(final String input) {
                      return OffsetTime.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class DurationParam extends GuardrailAbstractParam<Duration> {
                  public DurationParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected Duration realParse(final String input) {
                      return Duration.parse(input);
                  }
              }

              @Deprecated
              public static class Binder extends AbstractBinder {
                  @Override
                  protected void configure() {}
              }

              @Deprecated
              public static class Bundle implements io.dropwizard.Bundle {
                  @Override
                  public void initialize(final Bootstrap<?> bootstrap) {}

                  @Override
                  public void run(final Environment environment) {}
              }
          }

          private GuardrailJerseySupport() {}
      }
    """
  )

  def jacksonFormUtils: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "GuardrailJacksonFormUtils",
    """
      import com.fasterxml.jackson.databind.BeanDescription;
      import com.fasterxml.jackson.databind.JavaType;
      import com.fasterxml.jackson.databind.JsonNode;
      import com.fasterxml.jackson.databind.ObjectMapper;
      import com.fasterxml.jackson.databind.introspect.BasicClassIntrospector;
      import com.fasterxml.jackson.databind.introspect.BeanPropertyDefinition;
      import com.fasterxml.jackson.databind.introspect.ClassIntrospector;
      import com.fasterxml.jackson.databind.introspect.SimpleMixInResolver;
      import com.fasterxml.jackson.databind.node.ArrayNode;
      import com.fasterxml.jackson.databind.node.ObjectNode;
      import com.fasterxml.jackson.databind.node.TextNode;

      import java.lang.reflect.Type;
      import java.util.ArrayList;
      import java.util.Collections;
      import java.util.HashMap;
      import java.util.Iterator;
      import java.util.List;
      import java.util.Map;
      import java.util.Optional;
      import java.util.function.Function;
      import java.util.function.Supplier;
      import java.util.stream.Collectors;

      public class GuardrailJacksonFormUtils {
          public static <T> T readUrlEncodedForm(final ObjectMapper mapper,
                                                 final Type outputType,
                                                 final Supplier<Map<String, List<String>>> formDataReader)
          {
              final JavaType jType = mapper.constructType(outputType);
              final Map<String, BeanPropertyDefinition> properties = getProperties(mapper, jType);
              final Map<String, List<String>> formData = formDataReader.get();
              final JsonNode root = createFormDataNode(mapper, formData, properties);
              return mapper.convertValue(root, jType);
          }

          public static <T> Map<String, List<String>> writeUrlEncodedForm(final ObjectMapper mapper, final T data) {
              final JavaType jType = mapper.constructType(data.getClass());
              final Map<String, BeanPropertyDefinition> properties = getProperties(mapper, jType);
              final JsonNode root = mapper.convertValue(data, JsonNode.class);
              return createFormDataMap(root, properties);
          }

          private static Map<String, BeanPropertyDefinition> getProperties(final ObjectMapper mapper, final JavaType jType) {
              final ClassIntrospector classIntrospector = new BasicClassIntrospector();
              final BeanDescription desc = classIntrospector.forDeserialization(mapper.getDeserializationConfig(), jType, new SimpleMixInResolver(null));
              return desc.findProperties().stream().collect(Collectors.toMap(BeanPropertyDefinition::getName, Function.identity()));
          }

          private static JsonNode createFormDataNode(final ObjectMapper mapper,
                                                     final Map<String, List<String>> formData,
                                                     final Map<String, BeanPropertyDefinition> properties)
          {
              final ObjectNode root = mapper.createObjectNode();
              for (final Map.Entry<String, List<String>> entry : formData.entrySet()) {
                  if (properties.containsKey(entry.getKey())) {
                      final BeanPropertyDefinition defn = properties.get(entry.getKey());
                      final JavaType propType = propType(defn);

                      if (propType.isCollectionLikeType()) {
                          if (propType.getRawClass() == List.class) {
                              if (!root.has(entry.getKey())) {
                                  root.set(entry.getKey(), mapper.createArrayNode());
                              }
                              final ArrayNode array = (ArrayNode) root.get(entry.getKey());
                              array.addAll(entry.getValue().stream().map(TextNode::new).collect(Collectors.toList()));
                          } else {
                              throw new IllegalArgumentException("Unsupported form-data type " + propType.getRawClass().getName() + " for property '" + entry.getKey() + "'");
                          }
                      } else if (!entry.getValue().isEmpty()) {
                          root.set(entry.getKey(), new TextNode(entry.getValue().get(0)));
                      }
                  }
              }

              return root;
          }

          private static Map<String, List<String>> createFormDataMap(final JsonNode root,
                                                                     final Map<String, BeanPropertyDefinition> properties)
          {
              if (!root.isObject()) {
                  throw new IllegalArgumentException("Form-data POJO is not serializable");
              }

              final Map<String, List<String>> map = new HashMap<>();
              final Iterator<String> fieldNames = root.fieldNames();
              while (fieldNames.hasNext()) {
                  final String fieldName = fieldNames.next();
                  if (properties.containsKey(fieldName)) {
                      final JsonNode field = root.get(fieldName);

                      if (!field.isNull()) {
                          final BeanPropertyDefinition defn = properties.get(fieldName);
                          final JavaType propType = propType(defn);

                          if (field.isArray()) {
                              final List<String> array = new ArrayList<>();
                              for (final JsonNode element : field) {
                                  array.add(element.asText());
                              }
                              map.put(fieldName, array);
                          } else if (field.isObject()) {
                              throw new IllegalArgumentException("Unsupported form-data type " + propType.getRawClass().getName() + " for property '" + fieldName + "'");
                          } else {
                              map.put(fieldName, Collections.singletonList(field.asText()));
                          }
                      }
                  }
              }

              return map;
          }

          private static JavaType propType(final BeanPropertyDefinition defn) {
              if (defn.getPrimaryType().getRawClass() == Optional.class) {
                  return defn.getPrimaryType().containedType(0);
              } else {
                  return defn.getPrimaryType();
              }
          }
      }
    """
  )

  def jacksonFormMessageReaderWriterDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "GuardrailJacksonFormMessageReaderWriter",
    """
      import com.fasterxml.jackson.databind.ObjectMapper;
      import org.glassfish.jersey.message.internal.AbstractFormProvider;

      import javax.inject.Singleton;
      import javax.validation.ValidationException;
      import javax.ws.rs.Consumes;
      import javax.ws.rs.Produces;
      import javax.ws.rs.WebApplicationException;
      import javax.ws.rs.core.MediaType;
      import javax.ws.rs.core.MultivaluedHashMap;
      import javax.ws.rs.core.MultivaluedMap;
      import java.io.IOException;
      import java.io.InputStream;
      import java.io.OutputStream;
      import java.io.UncheckedIOException;
      import java.lang.annotation.Annotation;
      import java.lang.reflect.ParameterizedType;
      import java.lang.reflect.Type;
      import java.util.ArrayList;
      import java.util.List;
      import java.util.Map;
      import java.util.Optional;
      import java.util.regex.Matcher;
      import java.util.regex.Pattern;
      import java.util.stream.Collectors;

      @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
      @Produces(MediaType.APPLICATION_FORM_URLENCODED)
      @Singleton
      public class GuardrailJacksonFormMessageReaderWriter extends AbstractFormProvider<Object> {
          private final ObjectMapper mapper;

          public GuardrailJacksonFormMessageReaderWriter(final ObjectMapper mapper) {
              this.mapper = mapper;
          }

          @Override
          public boolean isReadable(final Class<?> type, final Type genericType, final Annotation[] annotations, final MediaType mediaType) {
              return this.mapper.canDeserialize(this.mapper.constructType(genericType));
          }

          @Override
          public boolean isWriteable(final Class<?> type, final Type genericType, final Annotation[] annotations, final MediaType mediaType) {
              return this.mapper.canSerialize(type);
          }

          @Override
          public Object readFrom(final Class<Object> type, final Type genericType, final Annotation[] annotations, final MediaType mediaType, final MultivaluedMap<String, String> httpHeaders, final InputStream entityStream) throws WebApplicationException, IOException {
              try {
                  final Type containedType = unwrapOptionalType(genericType);
                  final Object result = GuardrailJacksonFormUtils.readUrlEncodedForm(this.mapper, containedType, () -> {
                      try {
                          return readFrom(new MultivaluedHashMap<>(), mediaType, true, entityStream);
                      } catch (final IOException e) {
                          throw new UncheckedIOException(e);
                      }
                  });
                  return containedType == genericType ? result : Optional.ofNullable(result);
              } catch (final UncheckedIOException e) {
                  throw e.getCause();
              } catch (final IllegalArgumentException e) {
                  throw new ValidationException("Invalid or missing form data", e.getCause() != null ? e.getCause() : e);
              }
          }

          @Override
          public void writeTo(final Object t, final Class<?> type, final Type genericType, final Annotation[] annotations, final MediaType mediaType, MultivaluedMap<String, Object> httpHeaders, final OutputStream entityStream) throws IOException, WebApplicationException {
              final Map<String, List<String>> map = GuardrailJacksonFormUtils.writeUrlEncodedForm(this.mapper, t);
              final MultivaluedMap<String, String> mvmap = map.entrySet().stream().collect(Collectors.toMap(
                      Map.Entry::getKey,
                      Map.Entry::getValue,
                      (a, b) -> {
                          final List<String> combined = new ArrayList<>(a.size() + b.size());
                          combined.addAll(a);
                          combined.addAll(b);
                          return combined;
                      },
                      MultivaluedHashMap::new
              ));
              writeTo(mvmap, mediaType, entityStream);
          }

          private Type unwrapOptionalType(final Type genericType) {
              if (genericType instanceof ParameterizedType && ((ParameterizedType) genericType).getRawType() == Optional.class) {
                  final Type[] typeArgs = ((ParameterizedType) genericType).getActualTypeArguments();
                  if (typeArgs.length == 1) {
                      return typeArgs[0];
                  }
              }
              return genericType;
          }
      }
    """
  )
}
