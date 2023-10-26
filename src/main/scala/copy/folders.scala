package copy

import cats.effect._
import cats.implicits._

import java.io.BufferedInputStream
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.Files

import scala.jdk.StreamConverters._
import java.io.InputStream
import java.io.OutputStream
import java.io.FileOutputStream
import java.io.FileInputStream

object CopyFolder extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _           <- IO.raiseWhen(args.length < 2)(new IllegalArgumentException("Need origin and destination files"))
      source      <- identify(Paths.get(args(0)))
      destination <- identify(Paths.get(args(1)))
      _           <- IO.raiseWhen(source.path.equals(destination.path))(new IllegalArgumentException("Origin and destination files are the same"))
      count       <- copy(source, destination)
      _           <- IO.println(s"Finished copying $count files")
    } yield ExitCode.Success

  def copy(source: IdentifiedPath, destination: IdentifiedPath): IO[Int] =
    for {
      siblings <- IO.blocking(Files.walk(source.path).toScala(LazyList))
      _        <- IO.whenA(source.isDirectory && !destination.exists)(IO.blocking(Files.createDirectory(destination.path)))
      count    <- copy(siblings, source, destination)
    } yield count

  def cutRoot(paths: LazyList[Path]): IO[LazyList[Path]] = paths match {
    case current #:: LazyList() => IO.pure(paths)
    case current #:: rest => IO.pure(rest)
  }

  def copy(siblings: LazyList[Path], source: IdentifiedPath, destination: IdentifiedPath): IO[Int] = siblings match {
    case current #:: rest =>
      for {
        currentDestination <- identify(resolveDestination(source, current, destination))
        _                  <- IO.println(s"Copying $current to ${currentDestination.path}")
        countCurrent       <- identify(current).flatMap(path => copySingle(path, currentDestination))
        countRest          <- copy(rest, source, destination)
      } yield countCurrent + countRest
    case _ => IO.pure(0)
  }

  def resolveDestination(source: IdentifiedPath, sibling: Path, destination: IdentifiedPath): Path =
    if (source.isDirectory) destination.path.resolve(source.path.relativize(sibling))
    else destination.path

  def identify(path: Path): IO[IdentifiedPath] = for {
    exists      <- IO.blocking(Files.exists(path))
    isDirectory <- IO.blocking(Files.isDirectory(path))
  } yield IdentifiedPath(path, exists, isDirectory)

  def copySingle(source: IdentifiedPath, destination: IdentifiedPath): IO[Int] =
    if (source.isDirectory)
      for {
        _ <- IO.unlessA(destination.exists)(IO.blocking(Files.createDirectory(destination.path)))
      } yield if (destination.exists) 0 else 1
    else
      for {
        bytes <- inputOutputStreams(source.path, destination.path).use { case (in, out) =>
          transmit(in, out, new Array[Byte](1024 * 10), 0L)
        }
        _     <- IO.println(s"${destination.path}: ${bytes} bytes")
      } yield 1

  def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
    for {
      amount <- IO.blocking(origin.read(buffer, 0, buffer.size))
      count  <- if (amount > -1) IO.blocking(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
                else IO.pure(acc) // End of read stream reached (by java.io.InputStream contract), nothing to write
    } yield count // Returns the actual amount of bytes transmitted

  def inputStream(f: Path): Resource[IO, FileInputStream] =
    Resource.make {
      IO.blocking(new FileInputStream(f.toFile())).onError(_ => IO.println(s"Could not open the file ${f.toAbsolutePath()}!"))
    } { inStream =>
      IO.blocking(inStream.close()).handleErrorWith(_ => IO.unit)
    }

  def outputStream(f: Path): Resource[IO, FileOutputStream] =
    Resource.make {
      IO.blocking(new FileOutputStream(f.toFile())).onError(_ => IO.println(s"Could not create the file ${f.toAbsolutePath()}!"))
    } { outStream =>
      IO.blocking(outStream.close()).handleErrorWith(_ => IO.unit)
    }

  def inputOutputStreams(in: Path, out: Path): Resource[IO, (InputStream, OutputStream)] =
    for {
      inStream  <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)

  case class IdentifiedPath(path: Path, exists: Boolean, isDirectory: Boolean)
}
