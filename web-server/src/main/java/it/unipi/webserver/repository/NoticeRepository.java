package it.unipi.webserver.repository;

import it.unipi.webserver.entity.Notice;
import it.unipi.webserver.entity.Player;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface NoticeRepository extends JpaRepository<Notice, Long> {

    List<Notice> findNoticesByPlayer(Player player);
    void deleteAllByPlayer(Player player);
    void deleteByNoticeId(Long noticeId);
}
