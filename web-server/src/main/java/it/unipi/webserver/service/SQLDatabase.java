package it.unipi.webserver.service;

import it.unipi.webserver.entity.Game;
import it.unipi.webserver.entity.Notice;
import it.unipi.webserver.entity.Player;
import it.unipi.webserver.repository.GameRepository;
import it.unipi.webserver.repository.NoticeRepository;
import it.unipi.webserver.repository.PlayerRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;

import javax.persistence.OptimisticLockException;
import javax.transaction.Transactional;
import java.util.Date;
import java.util.List;

@Service
public class SQLDatabase {
    @Autowired
    private PlayerRepository playerRepository;
    @Autowired
    private GameRepository gameRepository;
    @Autowired
    private NoticeRepository noticeRepository;

    public boolean addPlayer(String username, String password) {
        try {
            Player player = new Player(username, password);
            playerRepository.save(player);
        } catch(Exception e) {
            return false;
        }
        return true;
    }

    public Player getPlayer(String username) {
        return playerRepository.findPlayerByUserName(username);
    }

    public boolean addGame(String playerManager, String pitchName, Date gameDay, int time) {
        try {
            Player manager = playerRepository.findPlayerByUserName(playerManager);
            Game game = new Game(manager, pitchName, gameDay, time);
            gameRepository.save(game);
        } catch(Exception e) {
            return false;
        }
        return true;
    }

    public List<Game> browseGames(String username) {
       List<Game> games = gameRepository.findGameByGameDayIsAfter(new Date());
       Player player = playerRepository.findPlayerByUserName(username);
       games.removeIf(game -> !(game.isPlayerManager(username) ||
                game.participates(player)));
       return games;
    }

    public List<Game> bookableGames(String username) {
        List<Game> games = gameRepository.findGameByGameDayIsAfter(new Date());
        Player player = playerRepository.findPlayerByUserName(username);
        games.removeIf(game -> game.isPlayerManager(username) ||
                game.participates(player));
        return games;
    }

    public boolean bookGame(Long gameId, String username) {
        try {
            Player player = playerRepository.findPlayerByUserName(username);
            Game game = gameRepository.findGameByGameId(gameId);

            if(game.getNumberOfPlayers() == 10) {
                return false;
            }

            game.addPlayer(player);
            gameRepository.save(game);
        } catch(ObjectOptimisticLockingFailureException e) {
            return false;
        }
        return true;
    }

    public boolean unbookGame(Long gameId, String username) {
        try {
            Player player = playerRepository.findPlayerByUserName(username);
            Game game = gameRepository.findGameByGameId(gameId);
            game.removePlayer(player);
            gameRepository.save(game);
        } catch(ObjectOptimisticLockingFailureException e) {
            return false;
        }
        return true;
    }

    public boolean deleteGame(Long gameId) {
        try {
            Game game = gameRepository.findGameByGameId(gameId);
            deleteGame(game);
            for(Player p: game.getPlayers()) {
                if(!game.isPlayerManager(p.getUserName())) {
                    postNotification(p.getUserName(), "Match deleted: " + game + ".");
                }
            }
        } catch(ObjectOptimisticLockingFailureException e) {
            return false;
        }
        return true;
    }

    @Transactional
    public void deleteGame(Game game) throws ObjectOptimisticLockingFailureException {
        // Made in this way otherwise Optimistic Locking Exception is not raised
        gameRepository.delete(game);
    }

    public void postNotification(String username, String message) {
        Player player = playerRepository.findPlayerByUserName(username);
        Notice notice = new Notice(player, message);
        notice.setPlayer(player);
        noticeRepository.save(notice);
    }

    public List<Notice> loadNotifications(String username) {
        Player player = playerRepository.findPlayerByUserName(username);
        return noticeRepository.findNoticesByPlayer(player);
    }

    @Transactional
    public boolean deleteNotification(Long noticeId) {
        try {
            noticeRepository.deleteByNoticeId(noticeId);
        } catch(Exception e) {
            return false;
        }
        return true;
    }

}
