/*
Autores: Grupo LM8
Laís Gray
Luís Pinto
Erick Ozaki
*/

import Game.{Board, Coord2D, Stone, populateBoard, populateRows, randomMove}
import Game._
import Game.Stone.Stone
import scala.io.StdIn.readLine

import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class AtariGO extends Application {
  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Atari GO")
    val fxmlLoader = new FXMLLoader(getClass.getResource("gui.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)
    // Definir tamanho mínimo
    primaryStage.setMinWidth(600)
    primaryStage.setMinHeight(400)
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}


object Main  {
  def main(args: Array[String]): Unit = {
    // Tui.start()
    Application.launch(classOf[AtariGO], args: _*)
  }
}
