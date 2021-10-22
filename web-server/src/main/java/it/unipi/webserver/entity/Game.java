package it.unipi.webserver.entity;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Entity
@Getter
@Setter
@ToString
public class Game implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long gameId;

    @Version
    private Long version;

    private int time;
    private String playerManager;
    private String pitchName;

    @ManyToMany(fetch = FetchType.EAGER)
    @JsonManagedReference
    @JoinTable(name = "match_booking", joinColumns = {
            @JoinColumn(name = "gameId") }, inverseJoinColumns = { @JoinColumn(name = "playerId") })
    private List<Player> players;

    public Game(){
        this.players = new ArrayList<>();
    }
    public Game(Player playerManager, String pitchName, int time) {
        this.playerManager = playerManager.getUserName();
        this.time = time;
        this.pitchName = pitchName;
        this.players = new ArrayList<>();
        this.players.add(playerManager);
    }
    public void addPlayer(Player p){
        players.add(p);
    }
    public void removePlayer(Player p){
        players.remove(p);
    }
    public int getNumberOfPlayers() {
        return players.size();
    }
    public boolean isPlayerManager(String p) {
        return playerManager.equals(p);
    }
    public boolean participates(Player p) {
        return players.contains(p);
    }
}
