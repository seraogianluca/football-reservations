package it.unipi.webserver.configuration;

import it.unipi.webserver.entity.MyGames;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.web.context.WebApplicationContext;

@Configuration
public class SessionConfig {
    @Bean
    @Scope(
            value = WebApplicationContext.SCOPE_SESSION,
            proxyMode = ScopedProxyMode.TARGET_CLASS)
    public MyGames games() {
        return new MyGames();
    }
}
