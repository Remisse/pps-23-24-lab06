package ex2

object ConferenceReviewings:

  enum Question:

    case Relevance
    case Significance
    case Confidence
    case Final

  trait ConferenceReviewing:

    type Article = Int

    def loadReview(article: Article, relevance: Int, significance: Int, confidence: Int, finalRating: Int): Unit
    def loadReview(article: Article, ratings: Map[Question, Int]): Unit
    def orderedScores(article: Article, ratingType: Question): List[Int]
    def averageFinalScore(article: Article): Double
    def acceptedArticles: Set[Article]
    def sortedAcceptedArticles: List[(Article, Double)]
    def averageWeightedFinalScoreMap: Map[Article, Double]

  class ConferenceReviewingImpl extends ConferenceReviewing:

    private var reviews: List[(Article, Map[Question, Int])] = List()

    override def loadReview(
      article: Article,
      relevance: Int,
      significance: Int,
      confidence: Int,
      finalRating: Int
    ): Unit =
      reviews = (
        article,
        Map(
          Question.Relevance -> relevance,
          Question.Significance -> significance,
          Question.Confidence -> confidence,
          Question.Final -> finalRating
        )
      ) :: reviews

    override def loadReview(article: Article, ratings: Map[Question, Int]): Unit =
      require(ratings.keys.size == Question.values.length)
      reviews = (article, ratings) :: reviews

    override def orderedScores(article: Article, question: Question): List[Int] =
      reviews
        .collect { case r if r._1 == article => r._2(question) }
        .sortWith(Ordering.Int.lt)

    private def reviewsCount(a: Article): Int = reviews.count(_._1 == a)

    override def averageFinalScore(article: Article): Double =
      reviews
        .collect { case r if r._1 == article => r._2(Question.Final) }
        .sum
        .toDouble / reviewsCount(article)

    override def acceptedArticles: Set[Article] =
      sortedAcceptedArticles
        .map(_._1)
        .toSet

    override def sortedAcceptedArticles: List[(Article, Double)] =
      reviews
        .collect { case r if r._2(Question.Relevance) >= 8 => r._1 }
        .distinct
        .map(a => (a, averageFinalScore(a)))
        .filter(_._2 > 5)
        .sortBy(_._2)

    override def averageWeightedFinalScoreMap: Map[Article, Double] =
      reviews
        .map(_._1)
        .distinct
        .map(a =>
          (
            a,
            reviews
              .collect { case x if x._1 == a => x._2 }
              .foldLeft(0.0) { (s, q) => s + (q(Question.Confidence) * q(Question.Final)) / 10.0 } / reviewsCount(a)
          )
        )
        .toMap
