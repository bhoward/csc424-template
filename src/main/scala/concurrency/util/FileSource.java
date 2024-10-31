package util;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * A data source that reads from a local file.
 * 
 * @author bhoward
 */
public class FileSource implements Source {
  private String description;
  private String fileName;

  public FileSource(String description, String fileName) {
    this.description = description;
    this.fileName = fileName;
  }

  public String getDescription() {
    return description;
  }

  public InputStream getInputStream() throws IOException {
    return new FileInputStream(fileName);
  }
}