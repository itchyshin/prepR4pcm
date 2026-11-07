"""Check downloaded receipts and their source archive, without network access."""
import hashlib
import json
import sys
from pathlib import Path

root = Path(sys.argv[1])
read = lambda name: json.loads((root / name).read_text())
full = read("full-replay-receipt.json")
scale = read("scale-receipt.json")
independent = read("independent-verification.json")
live = read("live-final-process-receipt.json")
archive_hash = hashlib.sha256((root / "prepR4pcm-wcvp-prototype.tar.gz").read_bytes()).hexdigest()
assert full["source_archive_sha256"] == archive_hash == scale["source_archive_sha256"]
assert live["source_archive_sha256"] == archive_hash and live["exit"] == 0
assert full["mode"] == "integrated" and full["cascade_reused"] is False
assert full["mapping_identical_to_saved_replay"] and full["mapping_identical_with_tnrs"]
assert independent["result"] == "PASS"
counts = independent["counts"]
assert full["input_rows"] == counts["original_input_names"]
assert full["tree_tips"] == counts["original_tree_tip_rows"]
assert full["wcvp_exact_pairs"] == counts["expected_candidate_pairs"] == counts["observed_candidate_pairs"]
assert independent["independent_resolution_checks"]["mismatch_count"] == 0
assert independent["candidate_set_comparison"]["missing_count"] == 0
assert independent["candidate_set_comparison"]["extra_count"] == 0
assert independent["distinct_taxa_negative_control"]["injected_pair_rejected"]
assert full["simulated_tnrs_query_names"] == full["simulated_tnrs_response_rows"]
assert "SIMULATED" in full["tnrs_validation_kind"]
assert full["local_candidate_pairs"] - full["original_candidate_pairs"] == full["newly_visible_pairs"]
print("BOTANICAL_EVIDENCE_PASS")
