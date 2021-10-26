package it.unipi.webserver.repository;


import it.unipi.webserver.entity.Game;
import org.springframework.data.jpa.repository.JpaRepository;


public interface GameRepository extends JpaRepository<Game, Long> {

    Game findGameByGameId(Long gameId);
    void deleteByGameId(Long id);
    int deleteByGameIdAndVersion(Long id, Long version);

}
