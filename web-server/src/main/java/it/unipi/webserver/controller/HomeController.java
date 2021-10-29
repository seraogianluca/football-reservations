package it.unipi.webserver.controller;

import it.unipi.webserver.entity.*;
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
    @Autowired
    private MyNotices notices;

    private String getUsername() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        return authentication.getName();
    }

    private void loadGames(String username) {
        games.clear();
        games.addAll(database.browseGames(username));
    }

    private void loadNotifications(Model model, String username) {
        notices.update(database.loadNotifications(username));
        model.addAttribute("notices", notices);
    }

    private void setUiResponse(Model model, String msg) {
        model.addAttribute("message", msg);
    }

    private void loadMainPage(Model model, String username) {
        loadGames(username);
        loadNotifications(model, username);
        model.addAttribute("fragment", "main");
        model.addAttribute("games", games);
    }

    private void loadDashboard(Model model, String username) {
        //loadGames(username);
        loadNotifications(model, username);
        model.addAttribute("fragment", "dashboard");
        model.addAttribute("games", games);
    }

    private void loadMessages(Model model, List<Message> messages, String active) {
        model.addAttribute("messages", messages);
        model.addAttribute("newmessage", new Message());
        model.addAttribute("activeGame", active);
    }

    @GetMapping(path="/match")
    public String addMatchPage(Model model) {
        loadNotifications(model, getUsername());
        model.addAttribute("fragment", "newmatch");
        model.addAttribute("match", new Game());
        return "home";
    }

    @PostMapping(path="/match/create")
    public String addGame(Model model, @ModelAttribute(value="match") Game match) {
        String username = getUsername();

        if(!database.addGame(username, match.getPitchName(), match.getTime())) {
            setUiResponse(model, "Sorry, something wrong occurs. Please try again.");
        } else {
            setUiResponse(model, "Match successfully created.");
        }

        loadGames(username);
        loadNotifications(model, username);
        model.addAttribute("fragment", "newmatch");
        return "home";
    }

    @GetMapping(path="/games")
    public String browseMyGames(Model model) {
        loadMainPage(model, getUsername());
        return "home";
    }

    @GetMapping(path="/search")
    public String browseBookableGames(Model model) {
        String username = getUsername();
        List<Game> games = database.bookableGames(username);
        loadNotifications(model, username);
        model.addAttribute("fragment", "search");
        model.addAttribute("games", games);
        return "home";
    }

    @PostMapping(path = "/games/unbook")
    public String unbookGame(Model model, @RequestParam("id") Long gameId) {
        String username = getUsername();

        if(!database.unbookGame(gameId, username)) {
            setUiResponse(model, "Sorry, something wrong occurs during unbooking. Please try again.");
        } else {
            setUiResponse(model, "Match unbooked successfully.");
        }

        loadMainPage(model, username);
        return "home";
    }

    @PostMapping(path="/search/book")
    public String bookGame(Model model, @RequestParam("id") Long gameId) {
        String username = getUsername();

        if(!database.bookGame(gameId, username)) {
            setUiResponse(model, "Sorry, something wrong occurs during booking or the match is full. Please try again.");
        } else {
            setUiResponse(model, "Match successfully booked.");
        }

        loadGames(username);
        loadNotifications(model, username);
        model.addAttribute("fragment", "search");
        return "home";
    }

    @PostMapping(path = "/games/delete")
    public String deleteGame(Model model, @RequestParam("id") Long gameId) {
        if(database.deleteGame(gameId)) {
            if(dashboardClient.deleteDashboard(Long.toString(gameId)))
                setUiResponse(model,"Match successfully deleted.");
        } else {
            setUiResponse(model,"Sorry, something wrong occurs during deleting. Please try again.");
        }

        loadMainPage(model, getUsername());
        return "home";
    }

    @PostMapping(path="/notice/delete")
    public String deleteNotifications(Model model, @RequestParam("id") Long noticeId){
        if(database.deleteNotification(noticeId)) {
            setUiResponse(model, "Notification successfully deleted.");
        } else {
            setUiResponse(model, "Sorry, something wrong occurs. Please try again.");
        }

        loadMainPage(model, getUsername());
        return "home";
    }

    @GetMapping(path = "/dashboard")
    public String loadDashboard(Model model) {
        loadDashboard(model, getUsername());
        return "home";
    }

    @PostMapping(path = "/dashboard/insert")
    public String insertMessage(Model model,
                                @ModelAttribute("newmessage") Message msg,
                                @RequestParam("id") String gameId) {
        List<Message> messages = dashboardClient.insertMessage(gameId, getUsername(), msg.getMessage());
        if(messages != null) {
            loadMessages(model, messages, gameId);
        } else {
            setUiResponse(model, "Sorry, the match has been deleted by the owner.");
        }

        loadDashboard(model);
        return "home";
    }

    @PostMapping(path = "/dashboard/read")
    public String readMessage(Model model, @RequestParam("id") String gameId){
        List<Message> messages = dashboardClient.readMessages(gameId);
        if(messages != null) {
            loadMessages(model, messages, gameId);
        } else {
            setUiResponse(model, "Sorry, the match has been deleted by the owner.");
        }

        loadDashboard(model);
        return "home";
    }

}
