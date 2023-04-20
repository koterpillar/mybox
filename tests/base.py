import os
from typing import Optional

CI: bool = "CI" in os.environ

DOCKER_IMAGE: Optional[str] = os.environ.get("DOCKER_IMAGE") or None

DOCKER: bool = DOCKER_IMAGE is not None
