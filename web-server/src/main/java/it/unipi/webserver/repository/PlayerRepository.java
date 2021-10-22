package it.unipi.webserver.repository;


import it.unipi.webserver.entity.Player;
import org.springframework.data.jpa.repository.JpaRepository;

public interface PlayerRepository extends JpaRepository<Player, Long> {

    Player findPlayerByUserName(String name);

}
