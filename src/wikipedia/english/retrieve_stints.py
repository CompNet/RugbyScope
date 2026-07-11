import subprocess
import sys

scripts = [
    "01_get_capped_players_urls.py",
    "02_02_download_all_capped_players_profiles.py",
    "03_download_all_wikidata_players.py",
    "04_compile_json_to_csv.py"
]

for script in scripts:
    print(f"Running {script}...")
    subprocess.run([sys.executable, script], check=True)

print("All scripts completed.")