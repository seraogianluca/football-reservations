package it.unipi.webserver.service;

import com.ericsson.otp.erlang.*;
import org.springframework.stereotype.Component;

import java.io.IOException;

@Component
public class DashboardClient {
    private OtpMbox mailBox;
    private final String serverNodeName = "dashboard@localhost";
    private final String serverName = "dashboard";

    public DashboardClient(){
        String nodeName = "dashboard_client@localhost";
        String mBoxName = "dashboard_client";
        String cookie = "dashboard";
        try{
            OtpNode node = new OtpNode(nodeName, cookie);
            mailBox = node.createMbox(mBoxName);

        }catch(IOException ex){
            ex.printStackTrace();
        }
    }

    public boolean insertMessage(String gameId, String username, String message){
        OtpErlangAtom insert = new OtpErlangAtom("insert");
        OtpErlangString game = new OtpErlangString(gameId);
        OtpErlangString user = new OtpErlangString(username);
        OtpErlangString msg = new OtpErlangString(message);

        OtpErlangTuple request = new OtpErlangTuple(new OtpErlangObject[]{insert, game, user, msg});
        OtpErlangTuple requestMsg = new OtpErlangTuple(new OtpErlangObject[]{mailBox.self(), request});

        System.out.println("[DBG] Request: " + requestMsg.toString());
        mailBox.send(serverName, serverNodeName, requestMsg);
        System.out.println("[DBG] Request sent.");
        try {
            OtpErlangTuple response = (OtpErlangTuple)mailBox.receive();
            String result = ((OtpErlangAtom) response.elementAt(1)).atomValue();
            return result.compareTo("success") == 0;
        } catch (Exception exception) {
            exception.printStackTrace();
        }
        return false;
    }
}
