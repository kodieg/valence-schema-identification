package kodie.phd.utils

import java.io.PrintWriter

/**
 * Created by kodie on 2/6/15.
 */
class HTMLReporter {

  def outputAligned(writer: PrintWriter, items: Seq[Seq[String]]) {
    if (!items.isEmpty) {
      val length = items.head.size
      writer.write("<table>")
      for (line <- items) {
        writer.write("<tr>")
        for (cell <- line) {
          if (cell.startsWith("<row>")) {
            writer.write(s"<td colspan=42 >$cell</td>")
          } else {
            writer.write(s"<td>$cell</td>")
          }
        }
        writer.write("</tr>")
      }
      writer.write("</table>")
    }
  }

}
