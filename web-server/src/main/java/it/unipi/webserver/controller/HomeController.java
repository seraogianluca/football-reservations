package it.unipi.webserver.controller;

import it.unipi.webserver.entity.Game;
import it.unipi.webserver.service.SQLDatabase;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.logout.SecurityContextLogoutHandler;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Controller
@RequestMapping("/home")
public class HomeController {
    @Autowired
    private SQLDatabase database;

    @GetMapping(path="/match/new")
    public String addMatchPage(Model model) {
        model.addAttribute("fragment", "newmatch");
        model.addAttribute("match", new Game());
        return "home";
    }

    @PostMapping(path="/match/new/add")
    public String addGame(Model model,
                          @ModelAttribute(value="match") Game match) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();

        String response = database.addGame( username,
                                            match.getPitchName(),
                                            match.getTime() );

        System.out.println("Response: " + response);
        model.addAttribute("fragment", "newmatch");
        model.addAttribute("message", response);
        return "home";
    }

    @GetMapping(path="/games")
    public String browseMyGames(Model model) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();
        List<Game> games = database.browseGames(username);

        model.addAttribute("fragment", "main");
        model.addAttribute("games", games);
        return "home";
    }

    @GetMapping(path="/games/search")
    public String browseBookableGames(Model model) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();
        List<Game> games = database.bookableGames(username);

        model.addAttribute("fragment", "search");
        model.addAttribute("games", games);
        return "home";
    }

    @PostMapping(path="/games/book")
    public String bookGame(Model model,
                           @RequestParam("book") String gameId) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();
        String response = database.bookGame(gameId, username);

        model.addAttribute("fragment", "search");
        model.addAttribute("message", response);
        return "home";
    }

}
