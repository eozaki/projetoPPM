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

    // Tamanho mínimo da janela
    primaryStage.setMinWidth(600)
    primaryStage.setMinHeight(400)

    primaryStage.setScene(scene)
    primaryStage.show()
  }
}
