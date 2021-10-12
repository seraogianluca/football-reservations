package it.unipi.webserver.controller;

import it.unipi.webserver.entity.Game;
import it.unipi.webserver.service.DashboardClient;
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
    @Autowired
    private DashboardClient dashboardClient;

    @GetMapping(path="/match")
    public String addMatchPage(Model model) {
        model.addAttribute("fragment", "newmatch");
        model.addAttribute("match", new Game());
        return "home";
    }

    @PostMapping(path="/match/create")
    public String addGame(Model model,
                          @ModelAttribute(value="match") Game match) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();

        String response = database.addGame( username,
                                            match.getPitchName(),
                                            match.getTime() );

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
                           @RequestParam("id") String gameId) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();
        String response = database.bookGame(gameId, username);

        model.addAttribute("fragment", "search");
        model.addAttribute("message", response);
        return "home";
    }

    @PostMapping(path = "/games/unbook")
    public String unbookGame(Model model,
                             @RequestParam("id") String gameId) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();
        String response = database.unbookGame(gameId, username);

        model.addAttribute("fragment", "main");
        model.addAttribute("message", response);
        return "home";
    }

    @PostMapping(path = "/games/delete")
    public String deleteGame(Model model,
                             @RequestParam("id") String gameId) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();
        String response = database.deleteGame(gameId);

        model.addAttribute("fragment", "main");
        model.addAttribute("message", response);
        return "home";
    }

    @GetMapping(path = "/dashboard/insert")
    public String insertMessage(Model model){
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();
        dashboardClient.insertMessage("2",username, "messaggio di prova");
        model.addAttribute("fragment", "main");
        return "home";
    }
}
