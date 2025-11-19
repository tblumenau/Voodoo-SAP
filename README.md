# Voodoo-SAP

Welcome! 👋  

This repository contains example ABAP code for integrating **Voodoo Robotics** wireless **pick-to-light / Cloud Display Devices** with **SAP** (ECC 6.0 and S/4HANA). The goal is simple:

> Make it easy for SAP teams to light up Voodoo devices from within their own ERP / WMS / EWM flows.

You are actively encouraged to **copy**, **modify**, **appropriate**, and **extend** anything in this repo. If something here saves you a few hours of trial-and-error, it has done its job.

---

## What this repo is

- A set of **ABAP examples** that show how to:
  - Call the Voodoo REST API from SAP
  - Trigger pick-to-light commands (single device, multiple devices)
  - Send static messages to devices
- A **table cross-reference** (`docs/voodoo_sap_tables.md`) mapping key **ECC 6.0** tables to their **S/4HANA** CDS / compatibility views for:
  - Inventory Management (IM)
  - Reservations / kitting inputs
  - Production orders
  - Warehouse tasks (WM/EWM)
  - Outbound deliveries and picking

This is not a polished product or official SAP add-on. It’s a **starting point** for your own integration.

---

## Who this is for

- SAP **ABAP developers** wiring up Voodoo Robotics devices
- **Solution architects** designing warehouse / kitting / pick-to-light flows
- **Integration partners** who want a concrete reference instead of a slide deck

If you live in PP / WM / EWM / IM / SD and want lights to blink when something needs to be picked, you’re in the right place.

---

## Repository contents

| File                                   | Description                                                     |
|----------------------------------------|-----------------------------------------------------------------|
| `abap/command_A.abap`                  | Simple example of sending a command to a single device.         |
| `abap/command_B.abap`                  | Variant command example (slightly different payload/flow).      |
| `abap/command_multiple_form.abap`      | Example of issuing a command to **multiple devices** at once.   |
| `abap/static_A.abap`                   | Static display example (show fixed text on a device).           |
| `abap/static_B.abap`                   | Alternative static example (different structure/format).        |
| `abap/static_C.abap`                   | Additional static scenario (e.g., different parameters).        |
| `abap/static_event.abap`               | Example tied to an event-style call (e.g., status display).     |
| `abap/static_single_form.abap`         | Single FORM version of a static display example.                |
| `docs/voodoo_sap_tables.md`            | ECC 6.0 → S/4HANA cross-reference for key logistics tables.     |
| `LICENSE`                              | MIT license (you can reuse this code broadly).                  |

> NOTE: File naming is intentionally simple. You are expected to rename / refactor to match your internal standards.

---

## Prerequisites

- Basic **ABAP** familiarity and access to an SAP development system
- Ability to make outbound HTTP calls from SAP (e.g., `CL_HTTP_CLIENT`)
- Your **Voodoo Robotics API base URL** and **API key**

---

## Getting started

1. **Clone or fork this repository**

   ```bash
   git clone https://github.com/tblumenau/Voodoo-SAP.git
   ```

2. **Create an ABAP program or include**
   - Create a new report or include in your SAP system.
   - Paste one of the sample `.abap` files into it.
   - Adjust names to match your namespace / package conventions.

3. **Configure the Voodoo endpoint**
   - Look for the placeholders in the code (for example):

     ```abap
     CONSTANTS: gc_base_url TYPE string VALUE 'https://YOUR-VOODOO-ENDPOINT',
                gc_api_key  TYPE string VALUE 'YOUR-API-KEY-HERE'.
     ```
   - Replace these with your own base URL and API key (or plug into your credential store / variant).

4. **Wire it into your process**
   - Typical integration points:
     - Production order picking (261 movements, `RESB` / `MATDOC` driven)
     - EWM warehouse tasks (`/SCWM/ORDIM_O`)
     - Outbound delivery picking (`LIPS`, `I_OutboundDeliveryItem`)
     - Kitting / One-Kit / staging processes
   - Use the examples as building blocks inside user-exits, BAdIs, function modules, or custom services.

### Simple end-to-end example

- Material is staged to a storage bin.
- Your custom logic determines the right Voodoo device for that bin.
- Call the sample code to trigger the pick light with quantity + message.
- Picker confirms, and you optionally send a second call to clear the device.

---

## How you can use this code

You are welcome to:

- Copy/paste snippets directly into your own code
- Fork this repo and maintain your own internal version
- Strip it down to the bare HTTP call and rebuild around it
- Use it as a reference for field mappings and payload structure

The code is licensed under MIT, so you can use it in commercial projects with minimal friction. See [LICENSE](LICENSE) for details.

---

## Contributing

Contributions are very welcome:

- Found a bug or typo? Open an Issue or send a Pull Request.
- Have a better example (e.g., EWM-specific or S/4-only)? Add a new `.abap` file under a clear name and mention it in the README.
- Want to document a full flow (e.g., “261 issue + light + confirmation”)? Add a short section to [`docs/voodoo_sap_tables.md`](docs/voodoo_sap_tables.md) or create a new doc under `docs/`.

When in doubt, small, focused PRs are easiest to review. See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines (keep it simple, no real customer data, follow ABAP naming best practices).

### Roadmap / Ideas

- Async callbacks for acknowledging picks
- EWM-specific command builders and warehouse task monitors
- BAPI / OData wrappers for exposing the API calls
- Extended examples for production order staging / reservations
- More detailed security / credential management samples

---

## Disclaimer

This repository is provided as-is:

- It is example code, not official SAP or Voodoo Robotics product code.
- You are responsible for reviewing and adapting it to your own security, performance, and coding standards.
- Never commit real customer data or credentials.

---

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for the legal text.

Happy hacking, and feel free to make these samples completely your own.
