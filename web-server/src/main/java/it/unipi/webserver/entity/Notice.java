package it.unipi.webserver.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;

@Entity
@Setter
@Getter
@NoArgsConstructor
public class Notice {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long noticeId;

    @ManyToOne(fetch = FetchType.LAZY)
    private Player player;

    private String noticeMessage;

    public Notice(Player p, String nm) {
        player = p;
        noticeMessage = nm;
    }
}