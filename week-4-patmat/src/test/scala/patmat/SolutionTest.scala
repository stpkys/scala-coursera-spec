package patmat

import org.scalatest.FunSuite

/**
  * Created by viktor on 24/09/2017.
  *
  */
class SolutionTest extends FunSuite {

  test("testSolution - case 1") {
    assert(Solution.solution("Sun 10:00-20:00\nFri 05:00-10:00\nFri 16:30-23:50\nSat 10:00-24:00\nSun 01:00-04:00\nSat 02:00-06:00\nTue 03:30-18:15\nTue 19:00-20:00\nWed 04:25-15:14\nWed 15:14-22:40\nThu 00:00-23:59\nMon 05:00-13:00\nMon 15:00-21:00") === 505)
    assert(Solution.solution("Sun 10:00-20:00") === 9240)
  }

  test("testSolution - case 2") {
    assert(Solution.solution("Mon 01:00-23:00\nTue 01:00-23:00\nWed 01:00-23:00\nThu 01:00-23:00\nFri 01:00-23:00\nSat 01:00-23:00\nSun 01:00-21:00") === 180)
    assert(Solution.solution("Mon 01:00-23:00\nTue 01:00-23:00\nWed 01:00-23:00\nThu 01:00-23:00\nFri 01:00-23:00\nSat 01:00-23:00\nSun 01:00-23:00") === 120)
    assert(Solution.solution("Mon 02:02-23:00\nTue 01:00-23:00\nWed 01:00-23:00\nThu 01:00-23:00\nFri 01:00-23:00\nSat 01:00-23:00\nSun 01:00-23:00") === 122)
    assert(Solution.solution("Mon 01:02-23:00\nTue 01:00-23:00\nWed 01:00-23:00\nThu 01:00-23:00\nFri 01:00-23:00\nSat 01:00-23:00\nSun 01:00-23:00") === 120)
  }

  test("timeParse") {
    assert(Solution.timeToMinutes("10:00") === 600)
    assert(Solution.timeToMinutes("00:00") === 0)
    assert(Solution.timeToMinutes("00:01") === 1)
    assert(Solution.timeToMinutes("11:11") === 671)
    assert(Solution.timeToMinutes("21:21") === 1281)
  }
}

