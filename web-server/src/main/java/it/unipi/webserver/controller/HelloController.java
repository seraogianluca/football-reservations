package it.unipi.webserver.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class HelloController {

    @Autowired

    @GetMapping(value="/pitches/home")
    public String home() {
        return "This is where we will show all the available pitches!";
    }
}
