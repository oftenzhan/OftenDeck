
echo "=== Appending to /boot/cmdline.txt ==="
echo -n " loglevel=1 quiet logo.nologo" | sudo tee -a /boot/cmdline.txt > /dev/null

echo "=== Appending to /boot/config.txt ==="
echo -e "\n# Disable rainbow splash screen\ndisable_splash=1" | sudo tee -a /boot/config.txt > /dev/null
echo -e "\n# Remove boot delay (less chance to enter recovery)\nboot_delay=0" | sudo tee -a /boot/config.txt > /dev/null

echo "Done. Reboot to see the changes."