package it.unipi.webserver.service;

import it.unipi.webserver.entity.Game;
import it.unipi.webserver.entity.Player;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.List;

@Service
public class SQLDatabase {

    private final WebClient database;

    @Autowired
    public SQLDatabase() {
        this.database = WebClient.builder().baseUrl("http://localhost:8080/").build();
    }

    //Add player
    //Add match
    //Book game

    /* GET PLAYER */
    public Player getPlayer(String username) {
        String requestPath = "player/get/" + username;
        return database.get()
                .uri(requestPath)
                .retrieve()
                .bodyToMono(Player.class)
                .block();
    }

    /* ADD MATCH */
    public String addGame(String playerManager, String pitchName, int time) {
        String requestPath = playerManager + "/" + pitchName + "/" + time;
        return database.post()
                .uri("game/add/" + requestPath)
                .retrieve()
                .bodyToMono(String.class)
                .block();
    }

    /* FIND ALL MY GAMES */
    public List<Game> browseGames(String username) {
        return database.get()
                .uri("game/get/user/" + username)
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<List<Game>>() {
                })
                .block();
    }

    /* FIND BOOKABLE GAMES */
    public List<Game> bookableGames(String username) {
        return database.get()
                .uri("game/get/all/" + username)
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<List<Game>>() {
                })
                .block();
    }

    /* BOOK A GAME */
    public String bookGame(String gameId, String player) {
        return database.put()
                .uri("game/update/" + gameId + "/" + player)
                .retrieve()
                .bodyToMono(String.class)
                .block();
    }

    /*
    @DeleteMapping(path="/delete/{name}")
    public String deletePitch(@PathVariable("name") String name){
        return this.webClientBuilder.build()
                .delete()
                .uri(baseUri + "/pitch/delete/" + name)
                .retrieve()
                .bodyToMono(String.class)
                .block();
    }

    @PutMapping(path="/update/{name}/{status}")
    public Boolean updateStatus(@PathVariable ("status") Boolean status
            , @PathVariable ("name") String name){
        return this.webClientBuilder.build()
                .put()
                .uri(baseUri + "/pitch/update/" + name + "/" + status)
                .retrieve()
                .bodyToMono(Boolean.class)
                .block();
    }
    */
}
