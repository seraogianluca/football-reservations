package it.unipi.webserver.repository;


import it.unipi.webserver.entity.Game;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Date;
import java.util.List;


public interface GameRepository extends JpaRepository<Game, Long> {

    Game findGameByGameId(Long gameId);
    Game findGameByPlayerManagerAndPitchName(String playerManager, String PitchName);

    @Query("select g from Game g where g.gameDay >= :today")
    List<Game> findGameByGameDayIsAfter(@Param("today") Date today);
}
