package it.unipi.webserver.service;

import it.unipi.webserver.entity.Game;
import it.unipi.webserver.entity.Player;
import it.unipi.webserver.repository.GameRepository;
import it.unipi.webserver.repository.PlayerRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.List;

@Service
public class SQLDatabase {
    @Autowired
    private PlayerRepository playerRepository;
    @Autowired
    private GameRepository gameRepository;

    public Player getPlayer(String username) {
        return playerRepository.findPlayerByUserName(username);
    }

    public String addGame(String playerManager, String pitchName, int time) {
        Player manager = playerRepository.findPlayerByUserName(playerManager);
        Game game = new Game(manager, pitchName, time);
        gameRepository.save(game);
        return "Match successfully created.";
    }

    public List<Game> browseGames(String username) {
        List<Game> games = gameRepository.findAll();
        Player player = playerRepository.findPlayerByUserName(username);
        games.removeIf(game -> !(game.isPlayerManager(username) ||
                game.participates(player)));
        return games;
    }

    public List<Game> bookableGames(String username) {
        List<Game> games = gameRepository.findAll();
        Player player = playerRepository.findPlayerByUserName(username);
        games.removeIf(game -> game.isPlayerManager(username) ||
                game.participates(player));
        return games;
    }

    public String bookGame(Long gameId, String username) {
        try {
            Player player = playerRepository.findPlayerByUserName(username);
            Game game = gameRepository.findGameByGameId(gameId);

            if(game.getNumberOfPlayers() == 10) {
                return "Sorry, this match is full.";
            }

            game.addPlayer(player);
            gameRepository.save(game);
        } catch(ObjectOptimisticLockingFailureException e) {
            return "Sorry, something wrong occurs during booking. Please try again.";
        }

        return "Match booked successfully.";
    }

    public String unbookGame(Long gameId, String username) {
        try {
            Player player = playerRepository.findPlayerByUserName(username);
            Game game = gameRepository.findGameByGameId(gameId);
            game.removePlayer(player);
            gameRepository.save(game);
        } catch(ObjectOptimisticLockingFailureException e) {
            return "Sorry, something wrong occurs during unbooking. Please try again.";
        }

        return "Match unbooked successfully.";
    }

    @Transactional
    public String deleteGame(Long gameId) {
        try {
            gameRepository.deleteByGameId(gameId);
        } catch(ObjectOptimisticLockingFailureException e){
            return "Sorry, something wrong occurs during deleting. Please try again.";
        }
        return "Match successfully deleted.";
    }

}
