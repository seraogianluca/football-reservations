package it.unipi.sqlserver.controller;

import it.unipi.sqlserver.entity.Game;

import it.unipi.sqlserver.entity.Player;
import it.unipi.sqlserver.repository.GameRepository;
import it.unipi.sqlserver.repository.PlayerRepository;
import org.hibernate.dialect.lock.OptimisticEntityLockException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
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

        return myGames;
    }

    @PostMapping(path = "/add/{playerManager}/{pitchName}/{time}")
    public String addGame(@PathVariable("playerManager") String playerManager,
                          @PathVariable("pitchName") String pitchName,
                          @PathVariable("time") int time
    ) {
        Game game= new Game();
        Player manager;
        manager = playerRepository.findPlayerByUserName(playerManager);
        game.setPlayerManager(playerManager);
        game.setPitchName(pitchName);
        game.setTime(time);
        game.addPlayer(manager);
        gameRepository.save(game);
        return "Match successfully created.";
    }

    @PutMapping(path="/update/{gameId}/{playerName}")
    public String bookGame(@PathVariable("gameId") Long gameId,
                            @PathVariable ("playerName") String playerName) {
        try {
            Player player = playerRepository.findPlayerByUserName(playerName);
            Game game = gameRepository.findGameByGameId(gameId);

            if(game.getPlayers().size() == 10) {
                return "Sorry, this match is full.";
            }

            game.addPlayer(player);
            gameRepository.save(game);
        } catch(ObjectOptimisticLockingFailureException e) {
            return "Sorry, something wrong occurs during booking. Please try again.";
        }

        return "Match booked successfully.";
    }

    @PutMapping(path="/update/unbook/{gameId}/{playerName}")
    public String unBookGame(@PathVariable("gameId") Long gameId,
                           @PathVariable ("playerName") String playerName){
        try {
            Game game = gameRepository.findGameByGameId(gameId);;
            Player player = playerRepository.findPlayerByUserName(playerName);

            if(playerName.equals("prova")) {
                Thread.sleep(5000);
            }

            game.removePlayer(player);
            gameRepository.save(game);
        } catch(ObjectOptimisticLockingFailureException e) {
            return "Sorry, something wrong occurs during unbooking. Please try again.";
        } catch(InterruptedException ex) {
            ex.printStackTrace();
        }
        return "Match unbooked successfully.";
    }

    @DeleteMapping(path= "/delete/{gameId}")
    @Transactional
    public String deleteGame(@PathVariable ("gameId") Long gameId){
        gameRepository.deleteByGameId(gameId);
        return "Game deleted!";
    }
}
