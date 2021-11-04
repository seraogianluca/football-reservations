package it.unipi.webserver.entity;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Entity
@Getter
@Setter
public class Game implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long gameId;

    @Version
    private Long version;

    private String playerManager;
    private String pitchName;

    @Temporal(TemporalType.DATE)
    private Date gameDay;

    private int time;

    @ManyToMany(fetch = FetchType.EAGER)
    @JsonManagedReference
    @JoinTable(name = "match_booking", joinColumns = {
            @JoinColumn(name = "gameId") }, inverseJoinColumns = { @JoinColumn(name = "playerId") })
    private List<Player> players;

    public Game(){
        this.players = new ArrayList<>();
    }
    public Game(Player playerManager, String pitchName, Date gameDay, int time) {
        this.gameDay = gameDay;
        this.time = time;
        this.playerManager = playerManager.getUserName();
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
    public String toString() {
        return pitchName + " at " + time;
    }
}
