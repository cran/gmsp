# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' @importFrom data.table .BY .I .N .SD CJ
#' @importFrom utils URLencode tail
NULL

# data.table column names and NSE aliases used in i, j and by.
# Functions and exported special symbols are imported above.
utils::globalVariables(c(
  ".", "..COLS", "..DampingRatios", "..Header", "..MasterHeader", "..Periods",
  "..Required", "AI", "AT", "ComponentID", "DIR", "EventDepth", "EventID",
  "EventLatitude", "EventLongitude", "EventMagnitude", "EventName", "H1", "H2", "ID",
  "IM", "IMF", "N", "NP", "OCID", "OCIDMAP", "OwnerID", "PGA", "PGA.final",
  "PGA.target.high", "PGA.target.low", "ProcessID", "Reason", "RecordID", "Repi", "Rhyp",
  "Sa", "Sa.high", "Sa.low", "Sa.mean", "StationID", "StationLatitude",
  "StationLongitude", "StationVs30", "Tn", "UP", "Y", "Year", "absent", "backup",
  "criterion", "dMean", "dt", "energyFraction", "existed", "field", "final", "i.AI",
  "i.EventMagnitude", "i.EventName", "i.H1", "i.H2", "i.N", "i.PGA", "i.Repi",
  "i.scaleFactor", "inside", "isArray", "isPGA", "k", "kind", "modeOrder", "p",
  "rank.band", "ratio", "required", "s", "scaleFactor", "stageFactor", "status",
  "target", "theta", "value", "violation", "xi", "zero"
))
