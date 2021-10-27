package it.unipi.webserver.entity;

import java.util.ArrayList;
import java.util.List;

public class MyNotices extends ArrayList<Notice> {
     public void update(List<Notice> noticeList) {
        this.clear();
        this.addAll(noticeList);
    }
}
