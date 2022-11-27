package cellulose

import java.io as ji

import language.experimental.captureChecking

object Printer:
  def print(out: ji.Writer, doc: Doc): Unit =
    
    def recur(node: Node, indent: Int): Unit = node match
      case Node(data, meta) =>
        data match
          case Data(key, children, layout, schema) =>
            for i <- 0 until indent do out.write(' ')
            out.write(key.s)
           
            schema match
              case Field(_, _) =>
                children.foreach:
                  case Node(Data(key, _, _, _), _) =>
                    out.write(' ')
                    out.write(key.s)
                out.write('\n')
              case Struct(_, _) =>
                children.take(layout.params).foreach:
                  case Node(Data(key, _, _, _), _) =>
                    out.write(' ')
                    out.write(key.s)
            
                out.write('\n')
                children.drop(layout.params).foreach(recur(_, indent + 2))
      
    doc.children.foreach(recur(_, doc.margin))