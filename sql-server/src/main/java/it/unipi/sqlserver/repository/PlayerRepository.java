package it.unipi.sqlserver.repository;


import it.unipi.sqlserver.entity.Player;
import org.springframework.data.jpa.repository.JpaRepository;

public interface PlayerRepository extends JpaRepository<Player, Long> {
    Player findPlayerByUserName(String name);

}
