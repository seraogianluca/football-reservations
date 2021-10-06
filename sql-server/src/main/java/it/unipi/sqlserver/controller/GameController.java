package it.unipi.sqlserver.controller;

import it.unipi.sqlserver.entity.Game;

import it.unipi.sqlserver.entity.Player;
import it.unipi.sqlserver.repository.GameRepository;
import it.unipi.sqlserver.repository.PlayerRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@RestController
@RequestMapping("game")
public class GameController {
    @Autowired
    private GameRepository gameRepository;
    @Autowired
    private PlayerRepository playerRepository;


    @PostMapping(path = "/add/{playerManager}/{pitchName}/{time}")
    public String addGame(@PathVariable("playerManager") String playerManager,
                          @PathVariable("pitchName") String pitchName,
                          @PathVariable("time") int time
    ){
        Game game= new Game();
        Player manager;
        manager = playerRepository.findPlayerByUserName(playerManager);
        game.setPlayerManager(playerManager);
        game.setPitchName(pitchName);
        game.setTime(time);
        game.addPlayer(manager);
        gameRepository.save(game);
        return "game added to pitch: " + pitchName;
    }
    @PutMapping(path="/update/{gameId}/{playerName}")
    public String bookGame(@PathVariable("gameId") Long gameId,
                            @PathVariable ("playerName") String playerName){

        Game game;
        Player player;
        game = gameRepository.findGameByGameId(gameId);
        if(game.getPlayers().size() > 9){ //TODO: gestire concorrenza!
            return "game full!";
        }
        player = playerRepository.findPlayerByUserName(playerName);
        game.addPlayer(player);
        gameRepository.save(game);

        return playerName + " has booked the game successfully!";
    }
    @PutMapping(path="/update/unbook/{gameId}/{playerName}")
    public String unBookGame(@PathVariable("gameId") Long gameId,
                           @PathVariable ("playerName") String playerName){

        Game game;
        Player player;
        game = gameRepository.findGameByGameId(gameId);

        player = playerRepository.findPlayerByUserName(playerName);
        game.removePlayer(player);
        gameRepository.save(game);

        return playerName + " has unbooked the game successfully!";
    }


    @ResponseBody
    @GetMapping(path = "/get/all/{userName}")
    public List<Game> browseMyBookableGames(@PathVariable("userName") String userName){
        List<Game> games;
        List<Game > notMyGames = new ArrayList<>();
        Player player;
        games = gameRepository.findAll();
        player = playerRepository.findPlayerByUserName(userName);

        for (Game game : games) {
            if (!game.getPlayerManager().equals(userName) &&
                    !game.getPlayers().contains(player)) {
                notMyGames.add(game);
            }
        }
        return notMyGames;

    }
    @ResponseBody
    @GetMapping(path = "/get/user/{userName}")
    public List<Game> browseMyGames(@PathVariable("userName") String userName){
        List<Game> games;
        List<Game > myGames = new ArrayList<>();
        Player player;
        games = gameRepository.findAll();
        player = playerRepository.findPlayerByUserName(userName);

        for (Game game : games) {
            if (game.getPlayerManager().equals(userName) ||
                    game.getPlayers().contains(player)) {
                myGames.add(game);
            }
        }
        System.out.println(Arrays.toString(myGames.toArray()));
        return myGames;

    }


    @DeleteMapping(path= "/delete/{gameId}")
    @Transactional
    public String deleteGame(@PathVariable ("gameId") Long gameId){
        gameRepository.deleteByGameId(gameId);
        return "Game deleted!";
    }
}
