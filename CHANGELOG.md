# Change Log and Release Notes

## 1.0.0 (Planned)

* Added ping endpoint to test service status.
* Removed CSV and usage dumping modules.
* Added route for fetching billing metrics.
* TODO: Remove lens.
* TODO: Reduce TTFB for fetching billing metrics.
* TODO: Add instructions for starting application.
* TODO: Dedup tag labels with the same name after sanitization.
* TODO: Calculate accurate costs with regard to usage quantities.
* TODO: Organize imports and dependencies.
* TODO: Review and optimize implementations.

## 0.4.3 (10 Dec 2018)

* Fixed binary copying for Docker image build.

## 0.4.2 (10 Dec 2018)

* Updated tests for billing gauge time and time labels changes.

## 0.4.1 (10 Dec 2018)

* Changed billing gauge time labels to derive from usage start time.

## 0.4.0 (10 Dec 2018)

* Unified exporter and gauges dump with ReaderT stack.
* Introduced mtl-style typeclasses to organize effects.
* Organized imports and standardized coding style.
* Reviewed and revamped various modules.
* Added and improved tests for various modules.
* Changed billing gauge time to usage start time.

## 0.3.1 (16 Nov 2018)

* Increased request timeout for getting rate card

## 0.3.0 (14 Nov 2018)

* Added date label to gauges.
* Sort output CSV rows by time.

## 0.2.4 (22 Oct 2018)

* Fixed Docker image default command.

## 0.2.3 (19 Oct 2018)

* Fixed Docker image default command.

## 0.2.2 (19 Oct 2018)

* Added default working directory for Docker image.

## 0.2.1 (19 Oct 2018)

* Fixed Docker image default command to server sub-command.

## 0.2.0 (19 Oct 2018)

* First public version.
