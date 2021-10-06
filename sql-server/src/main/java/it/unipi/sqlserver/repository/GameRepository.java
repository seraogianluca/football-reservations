package it.unipi.sqlserver.repository;


import it.unipi.sqlserver.entity.Game;
import org.springframework.data.jpa.repository.*;


public interface GameRepository extends JpaRepository<Game, Long> {

    Game findGameByGameId(Long gameId);
    void deleteByGameId(Long id);

}
