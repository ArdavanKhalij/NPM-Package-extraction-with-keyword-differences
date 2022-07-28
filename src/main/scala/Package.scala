import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case class Package(Name: String = "Empty", Dependencies: ListBuffer[Int] = ListBuffer[Int]()
                   , Version: ListBuffer[String] = ListBuffer[String]()
                   , DevDependencies: ListBuffer[Int] = ListBuffer[Int]()
                   , KeywordsDifferences: ListBuffer[String] = ListBuffer[String]()){
//  Get versions
  def get_json_and_versions: Package = {
//    Produce URL
    val url = s"https://registry.npmjs.org/${Name}"
//    Get response
    val response = requests.get(url)
//    Check if the API is available
    if (response.statusCode == 200) {
      val json = ujson.read(response.text)
//      Get the versions
      val versions = json.obj("versions").obj.toList
//      Make a list of versions
      for ((version, remain) <- versions){
        Version += version
      }
    }
    else {
      println(response.statusCode)
    }
    return this
  }
//  Get Dependencies
  def get_dependencies: Package = {
    var numberOfDependencies = 0
    val url = s"https://registry.npmjs.org/${Name}"
    var dependencies = ListBuffer()
    val response = requests.get(url)
    if (response.statusCode == 200) {
      val json = ujson.read(response.text)
      val versions = json.obj("versions").obj.toList
      for ((version, remain) <- versions) {
//        We use try catch because some packages has not dependency field
        try {
          var dependencies = remain.obj("dependencies").obj.toList
          numberOfDependencies += dependencies.length
        }
        catch {
          case e => numberOfDependencies += 0
        }
        Dependencies += numberOfDependencies
        numberOfDependencies = 0
      }
    }
    else {
      println(response.statusCode)
    }
    return this
  }
  //  Get Dev Dependencies
  def get_dev_dependencies: Package = {
    var numberOfDevDependencies = 0
    val url = s"https://registry.npmjs.org/${Name}"
    var dependencies = ListBuffer()
    val response = requests.get(url)
    if (response.statusCode == 200) {
      val json = ujson.read(response.text)
      val versions = json.obj("versions").obj.toList
      for ((version, remain) <- versions) {
        try {
          var devDependencies = remain.obj("devDependencies").obj.toList
          numberOfDevDependencies += devDependencies.length
        }
        catch {
          case e => numberOfDevDependencies += 0
        }
        DevDependencies += numberOfDevDependencies
        numberOfDevDependencies = 0
      }
    }
    else {
      println(response.statusCode)
    }
    return this
  }
  //  Get Keyword Differences
  def get_keyword_differences: Package = {
    val url = s"https://registry.npmjs.org/${Name}"
    val response = requests.get(url)
    var Keyword = ListBuffer[String]()
    val Keywords: ListBuffer[ListBuffer[String]] = ListBuffer[ListBuffer[String]]()
    if (response.statusCode == 200) {
      val json = ujson.read(response.text)
      val versions = json.obj("versions").obj.toList
      for ((version, remain) <- versions) {
        try {
          val keywords = remain.obj("keywords")
          val lenOfKey = keywords.toString().count(_ == ',') + 1
          var j = 0
          var keywords2: ListBuffer[String] = ListBuffer[String]()
          while (j < lenOfKey) {
            var k = keywords(j).toString()
            k = k.substring(1, k.length - 1)
            keywords2 += k
            j = j + 1
          }
          j = 0
          Keywords += keywords2
        }
        catch {
          case e => Keywords += ListBuffer()
        }
      }
      Keyword = Keywords(0)
      var ii = 1
      var output:String = ""
      Keyword.foreach(x => output = output + "+" + x + ", ")
      output = output.dropRight(2)
      output = "\"" + output + "\""
      KeywordsDifferences += output
      output = ""
      while (ii<Keywords.length){
        // + keywords
        val temp1 = Keywords(ii-1).toSet
        val result1 = Keywords(ii).filterNot(temp1)
        // - keywords
        val temp2 = Keywords(ii).toSet
        val result2 = Keywords(ii-1).filterNot(temp2)
        result1.foreach(x => output = output + "+" + x + ", ")
        result2.foreach(x => output = output + "-" + x + ", ")
        output = output.dropRight(2)
        output = "\"" + output + "\""
        KeywordsDifferences += output
        output = ""
        ii = ii + 1
      }
    }
    else{
      println("Connection failed!")
    }
    return this
  }
}
