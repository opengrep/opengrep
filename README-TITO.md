# Opengrep — Red Swarm Fork

**Fork of:** [opengrep/opengrep](https://github.com/opengrep/opengrep) v1.21.0
**Purpose:** Security scanning engine for the Red Swarm's TITO campaign pipeline
**License:** LGPL 2.1

## Why This Fork Exists

Opengrep is the community fork of Semgrep CE when Semgrep moved critical features (taint analysis, Apex/Elixir/VB support, etc.) behind a commercial license. This fork exists to:

1. **Track our customizations** — any TITO-specific changes to the engine itself
2. **Pin a known-good version** — v1.21.0 validated with our 105 custom rules + 2,144 registry rules
3. **Control our build pipeline** — we build and distribute for our aarch64 infra

## Our Combined Ruleset

The actual scanning rules live at `/opt/data/opengrep-rules/` on our infra node, combining:

| Source | Count | Description |
|--------|-------|-------------|
| Semgrep Community Registry | ~2,144 | 30+ language categories |
| TITO Custom Rules | 105 | MCP, LangChain, AI Agent, DeFi, AppSec |
| TITO Taint Rules | 10+ | Cross-function taint (`--taint-intrafile`) |
| **Combined** | **~2,260** | All validated with Opengrep v1.21.0 |

## Starting Point

- **Base tag:** `v1.21.0` (upstream)
- **TITO tag:** `tito-v1.21.0` (our fork starting point)
- **Default branch:** `main`

## Related

- [Opengrep SAST Scanner — Red Swarm Wiki](https://github.com/Leathal1/opengrep-fork)
- [TITO rules repo](https://github.com/Leathal1/TITO)
- [Daily scan cron](/opt/data/scripts/run-opengrep-scan.sh)
