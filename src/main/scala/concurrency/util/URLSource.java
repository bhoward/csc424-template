package util;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;

/**
 * A data source that reads from a web URL.
 * 
 * Based in part on code from David Eck,
 * http://math.hws.edu/eck/cs124/javanotes8/c11/s4.html
 * 
 * @author bhoward
 */
public class URLSource implements Source {
  private String description;
  private String urlString;

  public URLSource(String description, String urlString) {
    this.description = description;
    this.urlString = urlString;
  }

  public String getDescription() {
    return description;
  }

  public InputStream getInputStream() throws IOException {
    try {
      URL url = new URI(urlString).toURL();
      URLConnection connection = url.openConnection();
      return connection.getInputStream();
    } catch (URISyntaxException e) {
      e.printStackTrace();
      return null;
    }
  }
}