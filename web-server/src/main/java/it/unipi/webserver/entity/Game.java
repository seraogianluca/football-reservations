package it.unipi.webserver.entity;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@ToString
public class Game implements Serializable {

    private Long gameId;
    private int time;
    private String playerManager;
    private String pitchName;
    private List<Player> players;

    public Game(){
        this.players = new ArrayList<>();
    }
    public Game(Player playerManager, String pitchName, int time) {
        String name;
        name = playerManager.getUserName();
        this.playerManager = name;
        this.time = time;
        this.pitchName = pitchName;
        this.players.add(playerManager);

    }
    public void addPlayer(Player p){
        players.add(p);
    }
}
