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
        OtpErlangObject[] request = new OtpErlangObject[] {
                new OtpErlangTuple(new OtpErlangObject[] {
                        new OtpErlangAtom("insert"),
                        new OtpErlangString(gameId),
                        new OtpErlangString(username),
                        new OtpErlangString(message)
                }),
                new OtpErlangAtom("prova"),
                new OtpErlangAtom("provo")

        };
        System.out.println("la richiesta :" + request.toString());
        mailBox.send(serverName, serverNodeName, request);
        try {
            OtpErlangTuple response = (OtpErlangTuple) mailBox.receive();
            String result = ((OtpErlangAtom) response.elementAt(1)).atomValue();
            return result.compareTo("success") == 0;
        } catch (Exception exception) {
            exception.printStackTrace();
        }

        return false;
    }
}
