package it.unipi.webserver;

import it.unipi.webserver.entity.Game;
import it.unipi.webserver.repository.GameRepository;
import it.unipi.webserver.repository.PlayerRepository;
import it.unipi.webserver.service.SQLDatabase;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

@SpringBootTest
class WebServerApplicationTests {
    @Autowired
    SQLDatabase database;

    @Autowired
    GameRepository gameRepository;

    @Autowired
    PlayerRepository playerRepository;

    @Test
    void concurrentBooking() throws InterruptedException {
        // Populate player table
        assertThat(database.addPlayer("manager","manager")).isTrue();
        System.out.println("Player manager created.");

        for(int i = 0; i < 2; i++) {
            assertThat(database.addPlayer("player" + i, "player" + i)).isTrue();
            System.out.println("Player " + i + " created.");
        }

        // Create a match
        assertThat(database.addGame("manager", "somePitch", 20)).isTrue();
        System.out.println("Match created.");

        final Game srcGame = gameRepository.findGameByPlayerManagerAndPitchName("manager", "somePitch");
        assertThat(srcGame.getVersion()).isZero();
        System.out.println("Match version is zero.");

        // One should book successfully
        ExecutorService executorService = Executors.newFixedThreadPool(2);
        for(int i = 0; i < 2; i++) {
            String username = "player" + i;
            executorService.execute(
                    () -> {
                        System.out.println("Player " + username + " is trying to book.");
                        boolean ret = database.bookGame(srcGame.getGameId(), username);
                        System.out.println("Player " + username + " book result: " + ret);
                    }
            );
        }

        executorService.shutdown();
        executorService.awaitTermination(1, TimeUnit.MINUTES);

        Game game = gameRepository.findGameByGameId(srcGame.getGameId());
        assertThat(game.getVersion()).isEqualTo(1);
        System.out.println("Match version is one.");

        // Cleanup
        gameRepository.deleteAll();
        playerRepository.deleteAll();
    }

    @Test
    void overbookingTest() throws InterruptedException {
        // Populate player table
        assertThat(database.addPlayer("manager","manager")).isTrue();
        System.out.println("Player manager created.");

        for(int i = 0; i < 10; i++) {
            assertThat(database.addPlayer("player" + i, "player" + i)).isTrue();
            System.out.println("Player " + i + " created.");
        }

        // Create a match
        assertThat(database.addGame("manager", "somePitch", 20)).isTrue();
        System.out.println("Match created.");

        Game srcGame = gameRepository.findGameByPlayerManagerAndPitchName("manager", "somePitch");

        // One should book successfully
        ExecutorService executorService = Executors.newFixedThreadPool(2);
        for(int i = 0; i < 10; i++) {
            String username = "player" + i;
            System.out.println("Player " + username + " is trying to book.");
            boolean ret = database.bookGame(srcGame.getGameId(), username);
            System.out.println("Player " + username + " book result: " + ret);
        }

        Game game = gameRepository.findGameByGameId(srcGame.getGameId());
        assertThat(game.getNumberOfPlayers()).isEqualTo(10);
        System.out.println("Match has ten players.");

        // Cleanup
        gameRepository.deleteAll();
        playerRepository.deleteAll();
    }

}
