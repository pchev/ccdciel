# CCDCiel

A CCD capture software intended for the amateur astronomer.

CCDciel has all functionality required for advanced imaging of celestial objects including sequences for fully automated unattended operation. But it is also possible to make simple capture in manual mode.

For devices connection it uses the INDI and ASCOM standards drivers. It can control the CCD/CMOS camera, focuser, filter wheel, rotator and telescope mount and uses image resolving software such as Astrometry.net.


<img width="716" height="472" alt="image" src="https://github.com/user-attachments/assets/aa2a430e-99d3-4414-867c-8204fe5a8bc4" />

## Supported Platforms

- Windows
- macOS
- Linux

## Features

- Advanced automated imaging sequences for unattended operation
- Manual capture mode for interactive control
- INDI and ASCOM driver support
- Multi-device control:
  - CCD/CMOS cameras
  - Focusers
  - Filter wheels
  - Rotators
  - Telescope mounts
- Image plate-solving with Astrometry.net integration
- Cross-platform compatibility

## Status

The program is still in beta version but is now quite stable and runs smoothly on every platform. Testing and bug reports are welcome and appreciated.

## Technology

Built with:
- **Compiler:** Free Pascal
- **IDE & Library:** [Lazarus IDE](https://www.lazarus-ide.org/)

## Installation

For precompiled versions and detailed installation instructions, visit:
https://www.ap-i.net/ccdciel

## Requirements

- [libpasastro](https://github.com/pchev/libpasastro) - Required library

## Documentation & Support

- **Main Website:** https://www.ap-i.net/ccdciel
- **Issue Tracker:** https://www.ap-i.net/mantis/set_project.php?project_id=3
- **Standards & Drivers:**
  - [INDI Library](http://www.indilib.org/) - Instrument Neutral Distributed Interface
  - [ASCOM Standards](http://ascom-standards.org/) - Astronomy Common Object Model

## Related Projects

This software is part of a full suite for astronomical observation:

- [skychart](https://github.com/pchev/skychart) - Planetarium software
- [indistarter](https://github.com/pchev/indistarter) - INDI server management
- [eqmodgui](https://github.com/pchev/eqmodgui) - EQ mount control

## License

This project is licensed under the GNU General Public License v2.0 (GPLv2). See the LICENSE file in the repository for full details.

## Contributing

Contributions are welcome! Please report any issues, bugs, or suggestions through the [issue tracker](https://www.ap-i.net/mantis/set_project.php?project_id=3).

To contribute code:
1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request
