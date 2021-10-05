package it.unipi.webserver.controller;

import it.unipi.webserver.entity.Game;
import it.unipi.webserver.service.SQLDatabase;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

@Controller
@RequestMapping("/home")
public class HomeController {
    @Autowired
    private SQLDatabase database;

    @GetMapping(path="/pitches")
    public String getPitchesPage(Model model) {
        //List<Pitch> pitches = database.browsePitches();
        //model.addAttribute("pitches", pitches);
        model.addAttribute("message", "Hello pitches!");
        return "home";
    }

    @GetMapping(path="/match/new")
    public String addMatchPage(Model model) {
        model.addAttribute("fragment", "newmatch");
        model.addAttribute("match", new Game());
        return "home";
    }

    @PostMapping(path="/match/new/add")
    public String addGame(Model model,
                          @ModelAttribute(value="match") Game match) {

        String response = database.addGame( match.getPlayerManager(),
                                            match.getPitchName(),
                                            match.getTime() );

        System.out.println("Response: " + response);
        model.addAttribute("fragment", "newmatch");
        model.addAttribute("message", response);
        return "home";
    }

}
