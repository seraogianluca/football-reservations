package it.unipi.webserver.service;

import com.ericsson.otp.erlang.*;
import it.unipi.webserver.entity.Message;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

@Component
public class DashboardClient {
    private OtpMbox mailBox;
    private final String serverNodeName = "dashboard_server@localhost";
    private final String serverName = "dashboard_server";

    public DashboardClient(){
        try{
            OtpNode node = new OtpNode("dashboard_client@localhost", "dashboard");
            mailBox = node.createMbox("dashboard_client");
        }catch(IOException ex) {
            ex.printStackTrace();
        }
    }

    private String extractTimestamp(OtpErlangTuple tuple) {
        OtpErlangTuple date = (OtpErlangTuple)tuple.elementAt(0);
        OtpErlangTuple time = (OtpErlangTuple)tuple.elementAt(1);

        return  date.elementAt(2).toString() + "/" +
                date.elementAt(1).toString() + "/" +
                date.elementAt(0).toString() + " " +
                time.elementAt(0).toString() + ":" +
                time.elementAt(1).toString();
    }

    private void sendRequest(OtpErlangTuple requestBody) {
        OtpErlangTuple request = new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom("$gen_call"),
                new OtpErlangTuple(new OtpErlangObject[] {
                        this.mailBox.self(),
                        new OtpErlangAtom("nil")
                }),
                requestBody
        });

        mailBox.send(serverName, serverNodeName, request);
    }

    private OtpErlangTuple receiveResponse() {
        try {
            return (OtpErlangTuple)mailBox.receive(5000);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public boolean insertMessage(String gameId, String username, String message){
         sendRequest(new OtpErlangTuple(new OtpErlangObject[] {
                        new OtpErlangAtom("insert"),
                        new OtpErlangString(gameId),
                        new OtpErlangString(username),
                        new OtpErlangString(message)}));

        OtpErlangTuple response = receiveResponse();
        if(response == null) {
            return false;
        }
        String result = ((OtpErlangAtom)response.elementAt(1)).atomValue();
        return result.compareTo("success") == 0;
    }

    public boolean deleteMessages(String gameId) {
        sendRequest(new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom("delete"),
                new OtpErlangString(gameId)}));

        OtpErlangTuple response = receiveResponse();
        if(response == null) {
            return false;
        }
        String result = ((OtpErlangAtom)response.elementAt(1)).atomValue();
        return result.compareTo("success") == 0;
    }

    public List<Message> readMessages(String gameId) {
        sendRequest(new OtpErlangTuple(new OtpErlangObject[] {
                        new OtpErlangAtom("read"),
                        new OtpErlangString(gameId)}));

        OtpErlangTuple response = receiveResponse();
        List<Message> result = new ArrayList<Message>();
        if(response == null) {
            return result;
        }

        if (response.elementAt(1) instanceof OtpErlangList) {
            OtpErlangList resultList = (OtpErlangList)response.elementAt(1);
            for (OtpErlangObject otpErlangObject : resultList) {
                OtpErlangTuple tuple = (OtpErlangTuple)otpErlangObject;
                Message message = new Message(
                        tuple.elementAt(1).toString().substring(1, tuple.elementAt(1).toString().length()-1),
                        tuple.elementAt(2).toString().substring(1, tuple.elementAt(2).toString().length()-1),
                        extractTimestamp((OtpErlangTuple)tuple.elementAt(0)));
                result.add(message);
            }
        }

        return result;
    }

}
