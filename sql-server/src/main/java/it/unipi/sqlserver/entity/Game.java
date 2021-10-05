package it.unipi.sqlserver.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Time;
import java.sql.Timestamp;
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

    @ManyToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
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
}
