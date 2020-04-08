package core.Dropwizard;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import formPojos.client.dropwizard.definitions.SomeEnum;
import formPojos.client.dropwizard.formPojos.DoFormOptResponse;
import formPojos.client.dropwizard.formPojos.DoFormReqResponse;
import formPojos.client.dropwizard.formPojos.FormPojosClient;
import formPojos.server.dropwizard.GuardrailJacksonFormMessageReaderWriter;
import formPojos.server.dropwizard.definitions.FormBody;
import formPojos.server.dropwizard.formPojos.FormPojosHandler;
import formPojos.server.dropwizard.formPojos.FormPojosResource;
import io.dropwizard.testing.junit.ResourceTestRule;
import org.glassfish.jersey.test.TestProperties;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.junit.ClassRule;
import org.junit.Test;

import java.net.URI;
import java.net.URISyntaxException;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutionException;

import static org.assertj.core.api.Assertions.assertThat;

public class DropwizardFormPojosTest {
    static {
        System.setProperty(TestProperties.CONTAINER_PORT, "0");
    }

    private static final ObjectMapper mapper = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

    private static final FormPojosHandler handler = new FormPojosHandler() {
        @Override
        public CompletionStage<DoFormOptResponse> doFormOpt(final Optional<FormBody> body) {
            assertThat(body).isNotEmpty();
            return CompletableFuture.completedFuture(DoFormOptResponse.Ok(body.get()));
        }

        @Override
        public CompletionStage<DoFormReqResponse> doFormReq(final FormBody body) {
            return CompletableFuture.completedFuture(DoFormReqResponse.Ok(body));
        }
    };

    @ClassRule
    public static ResourceTestRule resources = new ResourceTestRule.Builder()
            .setTestContainerFactory(new GrizzlyTestContainerFactory())
            .addProvider(new GuardrailJacksonFormMessageReaderWriter(mapper))
            .addResource(new FormPojosResource(handler))
            .build();

    private static FormPojosClient createClient() throws URISyntaxException {
        final URI uri = resources.target("").getUriBuilder().build();
        return new FormPojosClient.Builder(new URI(uri.toString().substring(0, uri.toString().length() - 1)))
                .withObjectMapper(mapper)
                .build();
    }

    private formPojos.client.dropwizard.definitions.FormBody createFormBody() {
        return new formPojos.client.dropwizard.definitions.FormBody.Builder(
                "some string",
                42,
                false,
                OffsetDateTime.ofInstant(Instant.EPOCH, ZoneId.of("UTC")),
                SomeEnum.ANOTHER_VALUE
        )
                .withReqList(Arrays.asList("list item one", "list item two", "list item three"))
                .withOptString("another string")
                .withOptInt(9876)
                .withOptBoolean(true)
                .withOptDateTime(OffsetDateTime.ofInstant(Instant.EPOCH, ZoneId.of("UTC")))
                .withOptEnum(SomeEnum.YET_ANOTHER_VALUE)
                .build();
    }

    @Test
    public void testReqRoundTrip() throws ExecutionException, InterruptedException, URISyntaxException {
        final FormPojosClient client = createClient();
        final formPojos.client.dropwizard.definitions.FormBody body = createFormBody();
        final DoFormReqResponse response = client.doFormReq(body).call().toCompletableFuture().get();
        assertThat(response).isInstanceOf(DoFormReqResponse.Ok.class);
        assertThat(((DoFormReqResponse.Ok) response).getValue()).isEqualToComparingFieldByField(body);
    }

    @Test
    public void testOptRoundTrip() throws ExecutionException, InterruptedException, URISyntaxException {
        final FormPojosClient client = createClient();
        final formPojos.client.dropwizard.definitions.FormBody body = createFormBody();
        final DoFormOptResponse response = client.doFormOpt().withBody(body).call().toCompletableFuture().get();
        assertThat(response).isInstanceOf(DoFormOptResponse.Ok.class);
        assertThat(((DoFormOptResponse.Ok) response).getValue()).isEqualToComparingFieldByField(body);
    }
}
