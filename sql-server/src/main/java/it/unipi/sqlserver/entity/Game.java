package it.unipi.sqlserver.entity;

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
    public void removePlayer(Player p){
        players.remove(p);
    }
}
