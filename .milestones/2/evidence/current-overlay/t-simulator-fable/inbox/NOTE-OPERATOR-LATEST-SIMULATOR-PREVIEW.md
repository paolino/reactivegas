# Operator request: test the latest simulator

The operator asks: "can I test the latest simulator ?"

Please make the latest existing built candidate 48f76d96eb0975ec6c21cc5ba490af196d4882fa available as a temporary interactive preview, or verify an existing preview serves those exact assets. Reuse built output from the completed v17 gate. Return a usable operator URL, candidate identity, asset-to-build binding, and HTTP check in a short local handoff; acknowledge this note in your own STATUS. If the existing preview is older, do not describe it as latest.

This authorizes serving the existing candidate for operator testing, not production release, semantic changes, a new acceptance audit, or additional capped build invocations. C1 remains unaccepted and blocked on #92 for final CI. No need to wake the parked commit owner to serve existing output. Route a concrete obstacle if no built output can be made available without extra build work.

Deliver locally only; no text into the operator composer.
