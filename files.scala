import cats.effect._
import java.io._
import java.nio.file.Path
import java.nio.file.Paths

object CopyFile extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _     <- if (args.length < 2) IO.raiseError(new IllegalArgumentException("Need origin and destination files"))
               else IO.unit
      orig = Paths.get(args(0))
      dest = Paths.get(args(1))
      _     <- if (orig.equals(dest)) IO.raiseError(new IllegalArgumentException("Origin and destination files are the same"))
               else IO.unit
      count <- copy(orig, dest)
      _     <- IO.println(s"$count bytes copied from ${orig} to ${dest}")
    } yield ExitCode.Success

  def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
    for {
      amount <- IO.blocking(origin.read(buffer, 0, buffer.size))
      count  <- if (amount > -1) IO.blocking(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
                else IO.pure(acc) // End of read stream reached (by java.io.InputStream contract), nothing to write
    } yield count // Returns the actual amount of bytes transmitted // Returns the actual amount of bytes transmitted

  def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
    transmit(origin, destination, new Array[Byte](1024 * 10), 0L)

  def inputStream(f: Path): Resource[IO, FileInputStream] =
    Resource.make {
      IO.blocking(new FileInputStream(f.toFile()))                // build
    } { inStream =>
      IO.blocking(inStream.close()).handleErrorWith(_ => IO.unit) // release
    }

  def outputStream(f: Path): Resource[IO, FileOutputStream] =
    Resource.make {
      IO.blocking(new FileOutputStream(f.toFile()))                // build
    } { outStream =>
      IO.blocking(outStream.close()).handleErrorWith(_ => IO.unit) // release
    }

  def inputOutputStreams(in: Path, out: Path): Resource[IO, (InputStream, OutputStream)] =
    for {
      inStream  <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)

  def copy(origin: Path, destination: Path): IO[Long] = 
    inputOutputStreams(origin, destination).use { case (in, out) =>
      transfer(in, out)
    }

}
