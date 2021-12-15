package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.{assertEquals, assertNotEquals}

import scala.util.Random

class BlurSuite {
  def generate_img(sources: List[Img]): Unit = {
    for (src <- sources;
         row <- 0 until src.height;
         col <- 0 until src.width
         ) src(col, row) = row * src.width + col
  }

  /**
   * HorizontalBoxBlur / VerticalBoxBlur parBlur
   * should not forget the last strip
   */
  @Test def test_horizontal_last_strip(): Unit = {
    val radius = 1
    val src = new Img(4, 4)
    val dst = new Img(4, 4)
    generate_img(List(src, dst))
    HorizontalBoxBlur.parBlur(src, dst, 2, radius)
    // check every pixel of the last strip
    var result: List[Boolean] = List()
    for (
      row_num <- (src.height - src.height / 2) until src.height;
      column_num <- 0 until src.width
    ) result = result :+ (dst.apply(column_num, row_num) == src.apply
    (column_num, row_num))
    // check if all pixels of original and blured image are equal
    assertNotEquals("HorizontalBoxBlur: blured image must be " +
      "distinct from the original", true, result.forall(_ == true))
  }

  @Test def test_vertical_last_strip(): Unit = {
    val radius = 1
    val src = new Img(4, 4)
    val dst = new Img(4, 4)
    generate_img(List(src, dst))
    VerticalBoxBlur.parBlur(src, dst, 2, radius)
    // check every pixel of the last strip
    var result: List[Boolean] = List()
    for (
      column_num <- (src.width - src.width / 2) until src.width;
      row_num <- 0 until src.height
    ) result = result :+ (dst.apply(column_num, row_num) == src.apply
    (column_num, row_num))
    // check if all pixels of original and blured image are equal
    assertNotEquals("VerticalBoxBlur: blured image must be " +
      "distinct from the original", true, result.forall(_ == true))
  }

  /**
   * boxBlurKernel should return the correct value on
   * an interior pixel of a 3x4 image with radius 1
   * (використайте зручну вам кількість пікселів та радіус)
   */
  @Test def test_3x4_blur(): Unit = {
    val src = new Img(3, 4)
    generate_img(List(src))
    assertEquals(4,  boxBlurKernel(src, 1, 1, 1))
  }


  /**
   * boxBlurKernel should correctly handle radius 0
   */
  @Test def test_zero_radius(): Unit = {
    val radius = 0
    val src = new Img(8, 8)
    generate_img(List(src))
    assertEquals(src.apply(5, 5), boxBlurKernel(src, 5, 5, radius))
  }
  
  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
