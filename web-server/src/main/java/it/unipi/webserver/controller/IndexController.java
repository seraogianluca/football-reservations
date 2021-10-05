package it.unipi.webserver.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

@Controller
public class IndexController {

    @GetMapping(value="/")
    public String index() {
        return "index";
    }

    // TODO: Post method for authentication
    @GetMapping(value = "/home")
    public String playerHome(Model model) {
        model.addAttribute("fragment", "main");
        return "home";
    }

}
