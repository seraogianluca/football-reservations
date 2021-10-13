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
        OtpErlangTuple request = new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom("$gen_call"),
                new OtpErlangTuple(new OtpErlangObject[] {
                        this.mailBox.self(),
                        new OtpErlangAtom("nil")
                }),
                new OtpErlangTuple(new OtpErlangObject[] {
                        new OtpErlangAtom("insert"),
                        new OtpErlangString(gameId),
                        new OtpErlangString(username),
                        new OtpErlangString(message)
                })
        });

        mailBox.send(serverName, serverNodeName, request);

        try {
            OtpErlangTuple response = (OtpErlangTuple)mailBox.receive(5000);
            String result = ((OtpErlangAtom) response.elementAt(1)).atomValue();
            return result.compareTo("success") == 0;
        } catch (Exception exception) {
            exception.printStackTrace();
        }
        return false;
    }

    public List<Message> readMessages(String gameId) {
        OtpErlangTuple request = new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom("$gen_call"),
                new OtpErlangTuple(new OtpErlangObject[] {
                        this.mailBox.self(),
                        new OtpErlangAtom("nil")
                }),
                new OtpErlangTuple(new OtpErlangObject[] {
                        new OtpErlangAtom("read"),
                        new OtpErlangString(gameId)
                })
        });

        mailBox.send(serverName, serverNodeName, request);

        List<Message> result = null;
        try {
            OtpErlangTuple response = (OtpErlangTuple)mailBox.receive(5000);
            if (response.elementAt(1) instanceof OtpErlangList) {
                OtpErlangList resultList = (OtpErlangList)response.elementAt(1);
                result = new ArrayList<Message>();
                for (OtpErlangObject otpErlangObject : resultList) {
                    OtpErlangTuple tuple = (OtpErlangTuple)otpErlangObject;
                    Message message = new Message(tuple.elementAt(1).toString(),
                                                    tuple.elementAt(2).toString());
                    System.out.println("[DBG] message: " + message.getMessage());
                    result.add(message);
                }
            }
        } catch (Exception exception) {
            exception.printStackTrace();
        }
        return result;
    }
}
