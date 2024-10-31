package demo;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import util.FileSource;
import util.Source;
import util.URLSource;

public class IOBound {
  public static final int LONG_LENGTH = 12;

  /**
   * An example of an IO-bound computation that uses a cached thread pool to try
   * to keep busy while waiting for file or network access. Based on the "long
   * word count" example from Horstmann, Section 9.1.2, but with data sources
   * coming from both local files and web URLs.
   */
  public static void ioBound() {
    List<Source> sources = new ArrayList<>();
    sources.add(new FileSource("Alice in Wonderland", "data/alice30.txt"));
    sources.add(new FileSource("Count of Monte Cristo", "data/crsto10.txt"));
    sources.add(new FileSource("War and Peace", "data/war-and-peace.txt"));
    sources.add(new URLSource("Frankenstein", "https://www.gutenberg.org/files/84/84-0.txt"));
    sources.add(new URLSource("Pride and Prejudice", "https://www.gutenberg.org/files/1342/1342-0.txt"));
    sources.add(new URLSource("Great Gatsby", "https://www.gutenberg.org/files/64317/64317-0.txt"));

    ExecutorService service = Executors.newCachedThreadPool();
    List<Callable<Integer>> tasks = new ArrayList<>();
    for (Source source : sources) {
      tasks.add(() -> {
        System.out.println("Starting " + source.getDescription());

        try (BufferedReader reader = new BufferedReader(new InputStreamReader(source.getInputStream()))) {
          // Use the Stream library to process the lines from the source
          int result = reader.lines().mapToInt(line -> {
            String[] words = line.split("[\\PL]+");
            int count = 0;
            for (String word : words) {
              if (word.length() >= LONG_LENGTH)
                count++;
            }
            return count;
          }).sum();

          System.out.println("Ending " + source.getDescription());
          return result;
        } catch (IOException e) {
          e.printStackTrace();
          return 0;
        }
      });
    }

    // Add up all of the totals returned from the individual tasks
    try {
      List<Future<Integer>> resultFutures = service.invokeAll(tasks);

      int total = 0;
      for (Future<Integer> resultFuture : resultFutures) {
        total += resultFuture.get();
      }
      System.out.println("Found " + total + " long words");
    } catch (ExecutionException | InterruptedException e) {
      e.printStackTrace();
    }

    service.shutdown();
  }
}
