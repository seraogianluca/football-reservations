package it.unipi.webserver;

import it.unipi.webserver.service.SQLDatabase;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.web.client.RestTemplate;

import java.time.Duration;
import java.util.Arrays;

@SpringBootApplication
public class WebServerApplication {
    //@Autowired
    //private SQLDatabase sqlDatabase;

    public static void main(String[] args) {
        ApplicationContext context = SpringApplication.run(WebServerApplication.class, args);

        SQLDatabase sqlDatabase = context.getBean(SQLDatabase.class);
        System.out.println("result= "+ sqlDatabase.browsePitches());
    }

    @Bean
    public CommandLineRunner commandLineRunner(ApplicationContext ctx) {
        return args -> {

            System.out.println("Let's inspect the beans provided by Spring Boot:");

            String[] beanNames = ctx.getBeanDefinitionNames();
            Arrays.sort(beanNames);
            for (String beanName : beanNames) {
                System.out.println(beanName);
            }

        };
    }

}
