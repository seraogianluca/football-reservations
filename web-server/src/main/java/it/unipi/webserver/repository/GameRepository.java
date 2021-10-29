package it.unipi.webserver.repository;


import it.unipi.webserver.entity.Game;
import org.springframework.data.jpa.repository.JpaRepository;


public interface GameRepository extends JpaRepository<Game, Long> {

    Game findGameByGameId(Long gameId);
    Game findGameByPlayerManagerAndPitchName(String playerManager, String PitchName);
}
