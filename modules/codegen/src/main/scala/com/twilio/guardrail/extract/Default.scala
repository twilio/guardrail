package com.twilio.guardrail.extract

import com.twilio.guardrail.core.Tracker
import io.swagger.v3.oas.models.media.{ BooleanSchema, IntegerSchema, NumberSchema, StringSchema }
import io.swagger.v3.oas.models.parameters.Parameter

object Default {
  trait GetDefault[F] {
    def extract[T](from: F)(implicit T: Extractable[T]): Option[T]
  }

  object GetDefault {
    def build[F](f: F => Any): GetDefault[F] =
      new GetDefault[F] {
        def extract[T](from: F)(implicit T: Extractable[T]): Option[T] =
          T.extract(f(from)).toOption
      }

    implicit val getDefaultBooleanProperty: GetDefault[BooleanSchema] =
      build[BooleanSchema](_.getDefault)

    implicit val getDefaultNumberProperty: GetDefault[NumberSchema] =
      build[NumberSchema](_.getDefault)

    implicit val getDefaultIntegerProperty: GetDefault[IntegerSchema] =
      build[IntegerSchema](_.getDefault)

    implicit val getDefaultStringProperty: GetDefault[StringSchema] =
      build[StringSchema](_.getDefault)

    implicit val getDefaultQueryParameterParameter: GetDefault[Parameter] =
      build[Parameter](_.getSchema.getDefault)

    implicit def proxyDefaultForTracker[A](implicit ev: GetDefault[A]): GetDefault[Tracker[A]] =
      new GetDefault[Tracker[A]] {
        def extract[T](from: Tracker[A])(implicit T: Extractable[T]): Option[T] =
          ev.extract[T](from.unwrapTracker)
      }
  }

  case class DefaultAdapter[F](from: F) extends AnyVal {
    def extract[T: Extractable](implicit F: GetDefault[F]): Option[T] =
      F.extract(from)
  }

  def apply[F](from: F): DefaultAdapter[F] = DefaultAdapter(from)
}
