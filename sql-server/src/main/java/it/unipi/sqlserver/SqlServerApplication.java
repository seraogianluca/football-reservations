package it.unipi.sqlserver;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.remoting.rmi.RmiServiceExporter;

@SpringBootApplication
public class SqlServerApplication {

    public static void main(String[] args) {
        SpringApplication.run(SqlServerApplication.class, args);
    }

}
