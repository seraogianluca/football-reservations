package it.unipi.sqlserver.controller;

import it.unipi.sqlserver.entity.Game;

import it.unipi.sqlserver.entity.Player;
import it.unipi.sqlserver.repository.GameRepository;
import it.unipi.sqlserver.repository.PlayerRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

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
        player = playerRepository.findPlayerByUserName(playerName);
        game.addPlayer(player);
        gameRepository.save(game);

        return playerName + " has booked the game successfully!";
    }
}
