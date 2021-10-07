package it.unipi.sqlserver.controller;


import it.unipi.sqlserver.entity.Game;
import it.unipi.sqlserver.entity.Player;
import it.unipi.sqlserver.repository.GameRepository;
import it.unipi.sqlserver.repository.PlayerRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.transaction.Transactional;

@RestController
@RequestMapping("player")
public class PlayerController {
    @Autowired
    private PlayerRepository playerRepository;
    @Autowired
    private GameRepository gameRepository;


    @PostMapping(path = "/add/{userName}/{password}")
    public String addPlayer(@PathVariable("userName") String userName,
                            @PathVariable("password") String password
    ) {

        Player player = new Player();
        player.setUserName(userName);
        player.setPassword(password);
        playerRepository.save(player);
        return "player " + userName + "added to the system";
    }

    @DeleteMapping(path = "/delete/{playerId}")
    @Transactional
    public String deletePlayer(@PathVariable("playerId") Long playerId) {
        playerRepository.deleteById(playerId);

        return "Player deleted!";
    }

    @GetMapping(path = "/get/{userName}")
    public Player getUsername(@PathVariable("userName") String userName) {
        Player player;
        player = playerRepository.findPlayerByUserName(userName);
        return player;
    }

}
