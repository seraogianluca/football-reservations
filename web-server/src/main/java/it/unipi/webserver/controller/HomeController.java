package it.unipi.webserver.controller;

import it.unipi.webserver.entity.Game;
import it.unipi.webserver.entity.Message;
import it.unipi.webserver.entity.MyGames;
import it.unipi.webserver.entity.Notice;
import it.unipi.webserver.service.DashboardClient;
import it.unipi.webserver.service.SQLDatabase;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
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
    @Autowired
    private MyGames games;

    private void updateGame(String username) {
        games.clear();
        games.addAll(database.browseGames(username));
    }

    private void loadNotifications(Model model, String username) {
        List<Notice> notices = database.loadNotifications(username);
        model.addAttribute("notices", notices);
    }

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
        games.clear();
        games.addAll(database.browseGames(username));

        model.addAttribute("fragment", "newmatch");
        model.addAttribute("message", response);
        return "home";
    }

    @GetMapping(path="/games")
    public String browseMyGames(Model model) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();

        updateGame(username);
        loadNotifications(model, username);

        model.addAttribute("fragment", "main");
        model.addAttribute("games", games);
        return "home";
    }

    @PostMapping(path = "/games/unbook")
    public String unbookGame(Model model, @RequestParam("id") Long gameId) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();
        String response = database.unbookGame(gameId, username);

        updateGame(username);
        loadNotifications(model, username);

        model.addAttribute("fragment", "main");
        model.addAttribute("message", response);
        model.addAttribute("games", games);
        return "home";
    }

    @GetMapping(path="/search")
    public String browseBookableGames(Model model) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();
        List<Game> games = database.bookableGames(username);

        model.addAttribute("fragment", "search");
        model.addAttribute("games", games);
        return "home";
    }

    @PostMapping(path="/search/book")
    public String bookGame(Model model, @RequestParam("id") Long gameId) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();
        String response = database.bookGame(gameId, username);

        updateGame(username);

        model.addAttribute("fragment", "search");
        model.addAttribute("message", response);
        return "home";
    }

    @PostMapping(path = "/games/delete")
    public String deleteGame(Model model, @RequestParam("id") Long gameId) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();
        String response = database.deleteGame(gameId);
        dashboardClient.deleteMessages(Long.toString(gameId));

        updateGame(username);
        loadNotifications(model, username);

        model.addAttribute("fragment", "main");
        model.addAttribute("message", response);
        model.addAttribute("games", games);
        return "home";
    }

    @PostMapping(path = "/dashboard/insert")
    public String insertMessage(Model model,
                                @ModelAttribute("newmessage") Message msg,
                                @RequestParam("id") String gameId) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();

        dashboardClient.insertMessage(gameId, username, msg.getMessage());
        List<Message> messages = dashboardClient.readMessages(gameId);

        model.addAttribute("fragment", "dashboard");
        model.addAttribute("messages", messages);
        model.addAttribute("newmessage", new Message());
        model.addAttribute("activeGame", gameId);
        model.addAttribute("games", games);
        return "home";
    }

    @GetMapping(path = "/dashboard")
    public String loadDashboard(Model model) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();
        List<Game> games = database.browseGames(username);

        model.addAttribute("fragment", "dashboard");
        model.addAttribute("games", games);
        return "home";
    }

    @PostMapping(path = "/dashboard/read")
    public String readMessage(Model model, @RequestParam("id") String gameId){
        List<Message> messages = dashboardClient.readMessages(gameId);

        model.addAttribute("fragment", "dashboard");
        model.addAttribute("messages", messages);
        model.addAttribute("newmessage", new Message());
        model.addAttribute("activeGame", gameId);
        model.addAttribute("games", games);
        return "home";
    }
}
