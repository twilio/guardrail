package com.twilio.guardrail.protocol.terms.protocol

import cats.{ InjectK, Monad }
import cats.free.Free
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.SwaggerUtil

abstract class ArrayProtocolTerms[L <: LA, F[_]] {
  def MonadF: Monad[F]
  def extractArrayType(arr: SwaggerUtil.ResolvedType[L], concreteTypes: List[PropMeta[L]]): F[L#Type]
}

object ArrayProtocolTerms {
  implicit def arrayProtocolTerms[L <: LA, F[_]](implicit I: InjectK[ArrayProtocolTerm[L, ?], F]): ArrayProtocolTerms[L, Free[F, ?]] =
    new ArrayProtocolTerms[L, Free[F, ?]] {
      def MonadF = Free.catsFreeMonadForFree
      def extractArrayType(arr: SwaggerUtil.ResolvedType[L], concreteTypes: List[PropMeta[L]]): Free[F, L#Type] =
        Free.inject[ArrayProtocolTerm[L, ?], F](ExtractArrayType[L](arr, concreteTypes))
    }
}
