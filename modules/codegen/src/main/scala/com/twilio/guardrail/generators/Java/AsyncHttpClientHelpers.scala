package com.twilio.guardrail.generators.Java

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.twilio.guardrail.{ SupportDefinition, Target }
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage

object AsyncHttpClientHelpers {
  val DEFAULT_ASYNC_HTTP_CLIENT_CONFIG_BUILDER_TYPE: ClassOrInterfaceType = StaticJavaParser.parseClassOrInterfaceType("DefaultAsyncHttpClientConfig.Builder")
  val DEFAULT_ASYNC_HTTP_CLIENT_TYPE: ClassOrInterfaceType                = StaticJavaParser.parseClassOrInterfaceType("DefaultAsyncHttpClient")
  val ASYNC_HTTP_CLIENT_TYPE: ClassOrInterfaceType                        = StaticJavaParser.parseClassOrInterfaceType("AsyncHttpClient")
  val ASYNC_HTTP_CLIENT_CONFIG_TYPE: ClassOrInterfaceType                 = StaticJavaParser.parseClassOrInterfaceType("AsyncHttpClientConfig")
  val REQUEST_BUILDER_TYPE: ClassOrInterfaceType                          = StaticJavaParser.parseClassOrInterfaceType("RequestBuilder")
  val REQUEST_TYPE: ClassOrInterfaceType                                  = StaticJavaParser.parseClassOrInterfaceType("Request")
  val RESPONSE_TYPE: ClassOrInterfaceType                                 = StaticJavaParser.parseClassOrInterfaceType("Response")
  val INPUT_STREAM_PART_TYPE: ClassOrInterfaceType                        = StaticJavaParser.parseClassOrInterfaceType("InputStreamPart")
  val STRING_PART_TYPE: ClassOrInterfaceType                              = StaticJavaParser.parseClassOrInterfaceType("StringPart")

  def asyncHttpClientUtilsSupportDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "AsyncHttpClientUtils",
    """
      import com.fasterxml.jackson.databind.ObjectMapper;
      import org.asynchttpclient.Response;

      import java.io.UnsupportedEncodingException;
      import java.net.URLDecoder;
      import java.nio.charset.Charset;
      import java.nio.charset.StandardCharsets;
      import java.util.ArrayList;
      import java.util.Arrays;
      import java.util.HashMap;
      import java.util.List;
      import java.util.Locale;
      import java.util.Map;
      import java.util.Optional;

      public class AsyncHttpClientUtils {
          public static Optional<Charset> getResponseCharset(final Response response) {
              return Optional.ofNullable(response.getHeader("Content-Type")).flatMap(contentType -> {
                  final int charsetStart = contentType.toLowerCase(Locale.US).indexOf("charset=");
                  if (charsetStart != -1) {
                      final int charsetEnd = contentType.indexOf(";", charsetStart + 8);
                      final String charsetStr = contentType.substring(
                              charsetStart + 8,
                              charsetEnd != -1 ? charsetEnd : contentType.length());
                      try {
                          return Optional.of(Charset.forName(charsetStr));
                      } catch (final Exception e) {
                          return Optional.empty();
                      }
                  } else {
                      return Optional.empty();
                  }
              });
          }

          public static Map<String, List<String>> parseFormData(final Response response) {
              final Charset charset = getResponseCharset(response).orElse(StandardCharsets.UTF_8);
              final Map<String, List<String>> formData = new HashMap<>();
              Arrays.stream(response.getResponseBody().split("&")).forEach(pair -> {
                  final String[] parts = pair.split("=", 2);
                  if (!parts[0].equals("")) {
                      try {
                          final String key = URLDecoder.decode(parts[0], "UTF-8");
                          if (!formData.containsKey(key)) {
                              formData.put(key, new ArrayList<>());
                          }
                          if (parts.length == 2) {
                              formData.get(key).add(URLDecoder.decode(parts[1], "UTF-8"));
                          } else if (parts.length == 1) {
                              formData.get(key).add("true");
                          }
                      } catch (final UnsupportedEncodingException e) {
                          throw new RuntimeException(e.getMessage(), e);
                      }
                  }
              });
              return formData;
          }
      }
    """
  )
}
